;;; cursor-acp-transport.el --- Cursor ACP transport -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cursor-acp-core)
(require 'cursor-acp-ui)

(cl-defstruct (cursor-acp--terminal (:constructor cursor-acp--make-terminal))
  terminal-id
  session-id
  process
  log-path
  output
  truncated
  output-byte-limit
  exit-code
  signal
  waiters
  released)

(defvar cursor-acp--terminals nil
  "Hash table mapping terminalId -> `cursor-acp--terminal'.")

(defvar cursor-acp--terminal-next-seq 1)

(defun cursor-acp--ensure-terminals-table ()
  (unless (hash-table-p cursor-acp--terminals)
    (setq cursor-acp--terminals (make-hash-table :test #'equal)))
  cursor-acp--terminals)

(defun cursor-acp--terminal-id-next ()
  (let ((n cursor-acp--terminal-next-seq))
    (setq cursor-acp--terminal-next-seq (1+ n))
    (format "term_emacs_%d_%d" (truncate (float-time)) n)))

(defun cursor-acp--terminal--write-log (path text)
  (let ((dir (file-name-directory path)))
    (when (and (stringp dir) (not (file-directory-p dir)))
      (make-directory dir t)))
  (with-temp-buffer
    (insert (or text ""))
    (write-region (point-min) (point-max) path nil 'quiet)))

(defun cursor-acp--terminal--read-log (path)
  (if (and (stringp path) (file-readable-p path))
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-substring-no-properties (point-min) (point-max)))
    ""))

(defun cursor-acp--terminal--enforce-output-limit (s byte-limit)
  (let* ((limit (and (integerp byte-limit) (> byte-limit 0) byte-limit))
         (out (or s "")))
    (if (not limit)
        (cons out nil)
      (let ((b (string-bytes out)))
        (if (<= b limit)
            (cons out nil)
          (let* ((bytes-to-drop (- b limit))
                 (i 0))
            (while (and (< i (length out)) (> bytes-to-drop 0))
              (setq bytes-to-drop
                    (- bytes-to-drop
                       (string-bytes (char-to-string (aref out i)))))
              (setq i (1+ i)))
            (cons (substring out i) t)))))))

(defun cursor-acp--terminal--signal-name (n)
  (cond
   ((not (integerp n)) nil)
   ((fboundp 'signal-name)
    (ignore-errors (signal-name n)))
   (t (format "SIG%d" n))))

(defun cursor-acp--terminal--workspace-root-only (sess cwd)
  (let* ((root (file-name-as-directory (file-truename (cursor-acp--session-workspace-root sess))))
         (cw0 (if (and (stringp cwd) (not (string-empty-p (string-trim cwd))))
                  (file-name-as-directory (file-truename (expand-file-name cwd)))
                root)))
    (unless (string-equal cw0 root)
      (error "terminal/create cwd must be the workspace root: %s" root))
    root))

(defun cursor-acp--diff-block-p (obj)
  (and (hash-table-p obj)
       (let ((ty (cursor-acp--ht-get obj "type")))
         (and (stringp ty) (string-equal ty "diff")))
       (stringp (cursor-acp--ht-get obj "path"))
       (stringp (or (cursor-acp--ht-get obj "newText")
                    (cursor-acp--ht-get obj "new_text")))))

(defun cursor-acp--diff-block-fields (diff-ht)
  (let ((path (cursor-acp--ht-get diff-ht "path"))
        (old (or (cursor-acp--ht-get diff-ht "oldText")
                 (cursor-acp--ht-get diff-ht "old_text")))
        (new (or (cursor-acp--ht-get diff-ht "newText")
                 (cursor-acp--ht-get diff-ht "new_text"))))
    (list path (and (stringp old) old) (and (stringp new) new))))

(defun cursor-acp--collect-diff-blocks (obj)
  (let (out)
    (cl-labels
        ((walk (x)
           (cond
            ((cursor-acp--diff-block-p x)
             (push x out))
            ((hash-table-p x)
             (maphash (lambda (_k v) (walk v)) x))
            ((vectorp x)
             (mapc #'walk (append x nil)))
            ((listp x)
             (mapc #'walk x)))))
      (walk obj))
    (nreverse out)))

(defun cursor-acp--maybe-render-diffs-from (sess obj)
  (condition-case _
      (dolist (d (cursor-acp--collect-diff-blocks obj))
        (pcase-let ((`(,path ,old-text ,new-text) (cursor-acp--diff-block-fields d)))
          (when (and (stringp path) (stringp new-text))
            (condition-case _
                (let* ((root (cursor-acp--workspace-root sess))
                       (abs (if (file-name-absolute-p path)
                                (expand-file-name path)
                              (expand-file-name path root))))
                  (when (cursor-acp--path-in-workspace-p abs (cursor-acp--session-workspace-root sess))
                    (cursor-acp--review-ensure-seed sess abs (or old-text (cursor-acp--review-file-text abs))))
                  (cursor-acp--render-diff-block sess path (or old-text "") new-text))
              (error nil)))))
    (error nil)))

(defun cursor-acp--rpc-respond-error (sess id code message &optional data)
  (let* ((err `((code . ,code) (message . ,message)
                ,@(when data `((data . ,data)))))
         (msg `((jsonrpc . "2.0") (id . ,id) (error . ,err)))
         (json (cursor-acp--json-encode msg))
         (line (concat json "\n"))
         (proc (cursor-acp--conn-process (cursor-acp--ensure-conn))))
    (cursor-acp--log sess "-->" json)
    (process-send-string proc line)))

(defun cursor-acp--session-workspace-root (sess)
  "Return the workspace root directory for SESS.

Uses the session's main buffer `default-directory' when available, else falls
back to global `default-directory'."
  (let* ((mb (and (cursor-acp--valid-session-p sess)
                  (cursor-acp--session-main-buffer sess)))
         (dir (if (buffer-live-p mb)
                  (with-current-buffer mb default-directory)
                default-directory)))
    (file-name-as-directory (expand-file-name dir))))

(defun cursor-acp--path-in-workspace-p (path root)
  (let* ((p (file-truename (expand-file-name path)))
         (r (file-name-as-directory (file-truename (expand-file-name root)))))
    (string-prefix-p r p)))

(defun cursor-acp--fs-read-text-file (sess params)
  (let* ((path (and (hash-table-p params) (gethash "path" params nil)))
         (line (and (hash-table-p params) (gethash "line" params nil)))
         (limit (and (hash-table-p params) (gethash "limit" params nil)))
         (root (cursor-acp--session-workspace-root sess)))
    (unless (and (stringp path) (file-name-absolute-p path))
      (error "fs/read_text_file requires absolute path"))
    (unless (cursor-acp--path-in-workspace-p path root)
      (error "Path is outside workspace root: %s" root))
    (let* ((buf (get-file-buffer path))
           (content
            (cond
             ((buffer-live-p buf)
              (with-current-buffer buf
                (save-excursion
                  (save-restriction
                    (widen)
                    (goto-char (point-min))
                    (when (integerp line)
                      (when (< line 1) (error "line must be >= 1"))
                      (forward-line (1- line)))
                    (let ((beg (point)))
                      (when (integerp limit)
                        (when (< limit 0) (error "limit must be >= 0"))
                        (forward-line limit))
                      (buffer-substring-no-properties beg (point)))))))
             (t
              (with-temp-buffer
                (insert-file-contents path)
                (goto-char (point-min))
                (when (integerp line)
                  (when (< line 1) (error "line must be >= 1"))
                  (forward-line (1- line)))
                (let ((beg (point)))
                  (when (integerp limit)
                    (when (< limit 0) (error "limit must be >= 0"))
                    (forward-line limit))
                  (buffer-substring-no-properties beg (point))))))))
      `((content . ,content)))))

(defun cursor-acp--fs-write-text-file (sess params)
  (let* ((path (and (hash-table-p params) (gethash "path" params nil)))
         (content (and (hash-table-p params) (gethash "content" params nil)))
         (root (cursor-acp--session-workspace-root sess)))
    (unless (and (stringp path) (file-name-absolute-p path))
      (error "fs/write_text_file requires absolute path"))
    (unless (cursor-acp--path-in-workspace-p path root)
      (error "Path is outside workspace root: %s" root))
    (unless (stringp content)
      (error "fs/write_text_file requires string content"))
    (let ((seed (cursor-acp--review-file-text path)))
      (cursor-acp--review-ensure-seed sess path seed))
    (let ((parent (file-name-directory (expand-file-name path))))
      (when (and (stringp parent) (not (file-directory-p parent)))
        (make-directory parent t)))
    (with-temp-buffer
      (insert content)
      (write-region (point-min) (point-max) path nil 'quiet))
    nil))

(defun cursor-acp--terminal-get (terminal-id)
  (when (and (stringp terminal-id) (not (string-empty-p (string-trim terminal-id))))
    (gethash terminal-id (cursor-acp--ensure-terminals-table) nil)))

(defun cursor-acp--terminal--finalize-exit (term proc)
  (when (and (cursor-acp--terminal-p term) (processp proc))
    (let ((status (process-status proc)))
      (when (memq status '(exit signal))
        (setf (cursor-acp--terminal-exit-code term)
              (when (eq status 'exit)
                (ignore-errors (process-exit-status proc))))
        (setf (cursor-acp--terminal-signal term)
              (when (eq status 'signal)
                (cursor-acp--terminal--signal-name
                 (ignore-errors (process-exit-status proc)))))))))

(defun cursor-acp--terminal--respond-waiters (term)
  (when (cursor-acp--terminal-p term)
    (let ((waiters (cursor-acp--terminal-waiters term)))
      (setf (cursor-acp--terminal-waiters term) nil)
      (dolist (w waiters)
        (pcase-let ((`(,sess ,id) w))
          (when (and (cursor-acp--valid-session-p sess) (integerp id))
            (cursor-acp--rpc-respond
             sess id
             `((exitCode . ,(cursor-acp--terminal-exit-code term))
               (signal . ,(cursor-acp--terminal-signal term))))))))))

(defun cursor-acp--terminal--process-filter (term chunk)
  (when (and (cursor-acp--terminal-p term) (stringp chunk))
    (let* ((cur (or (cursor-acp--terminal-output term) ""))
           (combined (concat cur chunk))
           (limit (cursor-acp--terminal-output-byte-limit term))
           (pair (cursor-acp--terminal--enforce-output-limit combined limit))
           (new (car pair))
           (did-trunc (cdr pair))
           (log-path (cursor-acp--terminal-log-path term)))
      (setf (cursor-acp--terminal-output term) new)
      (when did-trunc
        (setf (cursor-acp--terminal-truncated term) t))
      (when (and (stringp log-path) (not (string-empty-p log-path)))
        (ignore-errors (cursor-acp--terminal--write-log log-path new))))))

(defun cursor-acp--terminal--process-sentinel (term proc _event)
  (cursor-acp--terminal--finalize-exit term proc)
  (cursor-acp--terminal--respond-waiters term))

(defun cursor-acp--terminal-create (sess params)
  (let* ((sid (and (hash-table-p params) (gethash "sessionId" params nil)))
         (command (and (hash-table-p params) (gethash "command" params nil)))
         (args0 (and (hash-table-p params) (gethash "args" params nil)))
         (env0 (and (hash-table-p params) (gethash "env" params nil)))
         (cwd0 (and (hash-table-p params) (gethash "cwd" params nil)))
         (limit (and (hash-table-p params) (gethash "outputByteLimit" params nil))))
    (unless (stringp sid) (error "terminal/create requires sessionId"))
    (unless (stringp command) (error "terminal/create requires command"))
    (let* ((root (cursor-acp--terminal--workspace-root-only sess cwd0))
           (args (cl-remove-if-not #'stringp (cursor-acp--normalize-items args0)))
           (env-items (cl-remove-if-not #'hash-table-p (cursor-acp--normalize-items env0)))
           (process-environment
            (append
             (mapcar
              (lambda (e)
                (let ((nm (cursor-acp--ht-get e "name"))
                      (val (cursor-acp--ht-get e "value")))
                  (unless (stringp nm) (error "terminal/create env.name must be string"))
                  (unless (stringp val) (setq val (format "%s" (or val ""))))
                  (format "%s=%s" nm val)))
              env-items)
             process-environment))
           (terminal-id (cursor-acp--terminal-id-next))
           (log-path (cursor-acp--terminal-log-file sess terminal-id))
           (term (cursor-acp--make-terminal
                  :terminal-id terminal-id
                  :session-id sid
                  :process nil
                  :log-path log-path
                  :output ""
                  :truncated nil
                  :output-byte-limit (and (integerp limit) (> limit 0) limit)
                  :exit-code nil
                  :signal nil
                  :waiters nil
                  :released nil)))
      (cursor-acp--terminal--write-log log-path "")
      (let* ((proc
              (let ((default-directory root))
                (make-process
                 :name (format "cursor-acp-terminal-%s" terminal-id)
                 :buffer nil
                 :command (cons command args)
                 :coding 'utf-8-unix
                 :connection-type 'pty
                 :noquery t
                 :filter (lambda (_p ch) (cursor-acp--terminal--process-filter term ch))
                 :sentinel (lambda (p ev) (cursor-acp--terminal--process-sentinel term p ev))))))
        (setf (cursor-acp--terminal-process term) proc)
        (puthash terminal-id term (cursor-acp--ensure-terminals-table))
        `((terminalId . ,terminal-id))))))

(defun cursor-acp--terminal-output-rpc (_sess params)
  (let* ((terminal-id (and (hash-table-p params) (gethash "terminalId" params nil)))
         (term (cursor-acp--terminal-get terminal-id)))
    (unless (cursor-acp--terminal-p term)
      (error "Unknown terminalId"))
    (when (cursor-acp--terminal-released term)
      (error "Terminal has been released"))
    (let* ((proc (cursor-acp--terminal-process term)))
      (when (and (processp proc) (memq (process-status proc) '(exit signal)))
        (cursor-acp--terminal--finalize-exit term proc)))
    (let* ((out (cursor-acp--terminal--read-log (cursor-acp--terminal-log-path term)))
           (exit (and (or (cursor-acp--terminal-exit-code term) (cursor-acp--terminal-signal term))
                      `((exitCode . ,(cursor-acp--terminal-exit-code term))
                        (signal . ,(cursor-acp--terminal-signal term))))))
      `((output . ,out)
        (truncated . ,(and (cursor-acp--terminal-truncated term) t))
        ,@(when exit `((exitStatus . ,exit)))))))

(defun cursor-acp--terminal-wait-for-exit (sess id params)
  (let* ((terminal-id (and (hash-table-p params) (gethash "terminalId" params nil)))
         (term (cursor-acp--terminal-get terminal-id)))
    (unless (cursor-acp--terminal-p term)
      (error "Unknown terminalId"))
    (when (cursor-acp--terminal-released term)
      (error "Terminal has been released"))
    (let ((proc (cursor-acp--terminal-process term)))
      (when (and (processp proc) (memq (process-status proc) '(exit signal)))
        (cursor-acp--terminal--finalize-exit term proc)))
    (if (or (cursor-acp--terminal-exit-code term) (cursor-acp--terminal-signal term))
        `((exitCode . ,(cursor-acp--terminal-exit-code term))
          (signal . ,(cursor-acp--terminal-signal term)))
      (setf (cursor-acp--terminal-waiters term)
            (append (cursor-acp--terminal-waiters term) (list (list sess id))))
      :cursor-acp--deferred)))

(defun cursor-acp--terminal-kill (sess params)
  (let* ((terminal-id (and (hash-table-p params) (gethash "terminalId" params nil)))
         (term (cursor-acp--terminal-get terminal-id)))
    (unless (cursor-acp--terminal-p term)
      (error "Unknown terminalId"))
    (when (cursor-acp--terminal-released term)
      (error "Terminal has been released"))
    (let ((proc (cursor-acp--terminal-process term)))
      (when (and (processp proc) (process-live-p proc))
        (ignore-errors (signal-process (process-id proc) 'SIGKILL))))
    nil))

(defun cursor-acp--terminal-release (sess params)
  (let* ((terminal-id (and (hash-table-p params) (gethash "terminalId" params nil)))
         (term (cursor-acp--terminal-get terminal-id)))
    (unless (cursor-acp--terminal-p term)
      (error "Unknown terminalId"))
    (unless (cursor-acp--terminal-released term)
      (let ((proc (cursor-acp--terminal-process term)))
        (when (and (processp proc) (process-live-p proc))
          (ignore-errors (signal-process (process-id proc) 'SIGKILL)))
        (when (processp proc)
          (ignore-errors (cursor-acp--terminal--finalize-exit term proc))))
      (setf (cursor-acp--terminal-released term) t)
      (cursor-acp--terminal--respond-waiters term)
      (remhash terminal-id (cursor-acp--ensure-terminals-table)))
    nil))

(defun cursor-acp--rpc-next-id (sess)
  (let* ((conn (cursor-acp--ensure-conn))
         (id (cursor-acp--conn-next-id conn)))
    (setf (cursor-acp--conn-next-id conn) (1+ id))
    id))

(defun cursor-acp--rpc-send (sess method params)
  (let* ((id (cursor-acp--rpc-next-id sess))
         (msg `((jsonrpc . "2.0") (id . ,id) (method . ,method) (params . ,params)))
         (json (cursor-acp--json-encode msg))
         (line (concat json "\n"))
         (proc (cursor-acp--conn-process (cursor-acp--ensure-conn))))
    (cursor-acp--log sess "-->" json)
    (process-send-string proc line)
    id))

(defun cursor-acp--rpc-notify (sess method params)
  (let* ((msg `((jsonrpc . "2.0") (method . ,method) (params . ,params)))
         (json (cursor-acp--json-encode msg))
         (line (concat json "\n"))
         (proc (cursor-acp--conn-process (cursor-acp--ensure-conn))))
    (cursor-acp--log sess "-->" json)
    (process-send-string proc line)))

(defun cursor-acp--rpc-respond (sess id result)
  (let* ((msg `((jsonrpc . "2.0") (id . ,id) (result . ,result)))
         (json (cursor-acp--json-encode msg))
         (line (concat json "\n"))
         (proc (cursor-acp--conn-process (cursor-acp--ensure-conn))))
    (cursor-acp--log sess "-->" json)
    (process-send-string proc line)))

(defun cursor-acp--rpc-send-async (sess method params &optional session-id)
  "Send a request and remember SESSION-ID for later response routing."
  (let* ((conn (cursor-acp--ensure-conn))
         (pending (cursor-acp--conn-pending conn))
         (id (cursor-acp--rpc-send sess method params)))
    (puthash id (list :done nil :result nil :error nil :session-id session-id) pending)
    id))

(defun cursor-acp--rpc-call (sess method params &optional timeout-seconds)
  (let* ((timeout (or timeout-seconds 20.0))
         (deadline (+ (float-time) timeout))
         (pending (cursor-acp--conn-pending (cursor-acp--ensure-conn)))
         (id (cursor-acp--rpc-send sess method params)))
    (puthash id (list :done nil :result nil :error nil) pending)
    (while (and (not (plist-get (gethash id pending) :done))
                (< (float-time) deadline)
                (process-live-p (cursor-acp--conn-process (cursor-acp--ensure-conn))))
      (accept-process-output (cursor-acp--conn-process (cursor-acp--ensure-conn)) 0.05))
    (let* ((state (gethash id pending))
           (done (and state (plist-get state :done)))
           (err (and state (plist-get state :error)))
           (res (and state (plist-get state :result))))
      (when state (remhash id pending))
      (cond
       ((not done) (error "ACP RPC timeout: %s" method))
       (err (error "ACP RPC error for %s: %S" method err))
       (t res)))))

(defun cursor-acp--terminal-stop-reason-p (sr)
  (and (stringp sr)
       (member sr '("end_turn" "cancelled" "canceled" "cancelled_by_user" "user_cancelled"))))

(defun cursor-acp--permission--normalize-tool-call-id (params)
  (let* ((tool-call (and (hash-table-p params) (cursor-acp--ht-get params "toolCall")))
         (raw (and (hash-table-p tool-call) (cursor-acp--ht-get tool-call "toolCallId"))))
    (when (stringp raw)
      (let* ((s (string-trim raw))
             (s1 (replace-regexp-in-string "[\r\n]+" "" s)))
        (unless (string-empty-p s1) s1)))))

(defun cursor-acp--handle-permission-request (sess id params)
  (let* ((tool-call-id (cursor-acp--permission--normalize-tool-call-id params))
         (cache (cursor-acp--session-permission-response-cache sess))
         (cached (and tool-call-id (hash-table-p cache) (gethash tool-call-id cache nil))))
    (cond
     (cached
      (cursor-acp--rpc-respond sess id `((outcome . ,cached))))
     ((and (cursor-acp--session-awaiting-permission sess)
           (stringp tool-call-id)
           (stringp (cursor-acp--session-permission-tool-call-id sess))
           (string-equal tool-call-id (cursor-acp--session-permission-tool-call-id sess)))
      (setf (cursor-acp--session-permission-request-ids sess)
            (append (cursor-acp--session-permission-request-ids sess) (list id))))
     ((and (cursor-acp--session-awaiting-permission sess)
           tool-call-id)
      (cursor-acp--rpc-respond sess id '((outcome . ((outcome . "cancelled")))))
      (cursor-acp--log sess "permission" (format "ignored while awaiting: toolCallId=%s" tool-call-id)))
     (t
      (setf (cursor-acp--session-permission-tool-call-id sess) tool-call-id)
      (setf (cursor-acp--session-permission-request-id sess) id)
      (setf (cursor-acp--session-permission-request-ids sess) (list id))
      (setf (cursor-acp--session-permission-request-params sess) params)
      (cursor-acp--set-awaiting-permission sess t)
      (cursor-acp--assistant-flush-frag sess)
      (condition-case _
          (let ((outcome (cursor-acp--prompt-permission-decision sess params))
                (ids (cursor-acp--session-permission-request-ids sess)))
            (when (and tool-call-id (hash-table-p cache))
              (puthash tool-call-id outcome cache))
            (dolist (rid ids)
              (when (integerp rid)
                (cursor-acp--rpc-respond sess rid `((outcome . ,outcome)))))
            (setf (cursor-acp--session-permission-tool-call-id sess) nil)
            (setf (cursor-acp--session-permission-request-id sess) nil)
            (setf (cursor-acp--session-permission-request-ids sess) nil)
            (setf (cursor-acp--session-permission-request-params sess) nil)
            (cursor-acp--set-awaiting-permission sess nil)
            (cursor-acp--render-info sess))
        (quit
         (cursor-acp--chat-insert sess "[permission prompt cancelled - use C-c C-a to retry]\n" t)
         (cursor-acp--render-info sess)))))))

(defun cursor-acp--cancel-pending-permission (sess)
  (when-let ((ids (or (cursor-acp--session-permission-request-ids sess)
                      (when-let ((rid (cursor-acp--session-permission-request-id sess)))
                        (list rid)))))
    (dolist (rid ids)
      (when (integerp rid)
        (cursor-acp--rpc-respond sess rid '((outcome . ((outcome . "cancelled")))))))
    (setf (cursor-acp--session-permission-tool-call-id sess) nil)
    (setf (cursor-acp--session-permission-request-id sess) nil)
    (setf (cursor-acp--session-permission-request-ids sess) nil)
    (setf (cursor-acp--session-permission-request-params sess) nil)
    (cursor-acp--set-awaiting-permission sess nil)
    (cursor-acp--chat-insert sess "[permission cancelled]\n" t)
    t))

(defun cursor-acp--reprompt-pending-permission (sess)
  (let ((rid (cursor-acp--session-permission-request-id sess))
        (params (cursor-acp--session-permission-request-params sess)))
    (unless (and rid (hash-table-p params))
      (user-error "No pending permission request"))
    (let* ((tool-call-id (cursor-acp--session-permission-tool-call-id sess))
           (cache (cursor-acp--session-permission-response-cache sess))
           (ids (or (cursor-acp--session-permission-request-ids sess) (list rid)))
           (outcome (cursor-acp--prompt-permission-decision sess params)))
      (when (and tool-call-id (hash-table-p cache))
        (puthash tool-call-id outcome cache))
      (dolist (rid0 ids)
        (when (integerp rid0)
          (cursor-acp--rpc-respond sess rid0 `((outcome . ,outcome)))))
      (setf (cursor-acp--session-permission-tool-call-id sess) nil)
      (setf (cursor-acp--session-permission-request-id sess) nil)
      (setf (cursor-acp--session-permission-request-ids sess) nil)
      (setf (cursor-acp--session-permission-request-params sess) nil)
      (cursor-acp--set-awaiting-permission sess nil)
      (cursor-acp--render-info sess))))

(defun cursor-acp--ask-question-tool-call-id-from-params (params)
  (let* ((raw (and (hash-table-p params) (cursor-acp--ht-get params "toolCallId"))))
    (when (stringp raw)
      (let* ((s (string-trim raw))
             (s1 (replace-regexp-in-string "[\r\n]+" "" s)))
        (unless (string-empty-p s1) s1)))))

(defun cursor-acp--handle-ask-question-request (sess id params)
  (let* ((tool-call-id (cursor-acp--ask-question-tool-call-id-from-params params))
         (cache (cursor-acp--session-ask-question-response-cache sess))
         (cached (and tool-call-id (hash-table-p cache) (gethash tool-call-id cache nil))))
    (cond
     (cached
      (cursor-acp--rpc-respond sess id `((outcome . ,cached))))
     ((and (cursor-acp--session-awaiting-ask-question sess)
           (stringp tool-call-id)
           (stringp (cursor-acp--session-ask-question-tool-call-id sess))
           (string-equal tool-call-id (cursor-acp--session-ask-question-tool-call-id sess)))
      (setf (cursor-acp--session-ask-question-request-ids sess)
            (append (cursor-acp--session-ask-question-request-ids sess) (list id))))
     ((and (cursor-acp--session-awaiting-ask-question sess)
           tool-call-id)
      (cursor-acp--rpc-respond sess id '((outcome . ((outcome . "cancelled")))))
      (cursor-acp--log sess "ask_question" (format "ignored while awaiting: toolCallId=%s" tool-call-id)))
     (t
      (setf (cursor-acp--session-ask-question-tool-call-id sess) tool-call-id)
      (setf (cursor-acp--session-ask-question-request-id sess) id)
      (setf (cursor-acp--session-ask-question-request-ids sess) (list id))
      (setf (cursor-acp--session-ask-question-request-params sess) params)
      (cursor-acp--set-awaiting-ask-question sess t)
      (cursor-acp--assistant-flush-frag sess)
      (condition-case _
          (let ((outcome (cursor-acp--prompt-ask-question-decision sess params))
                (ids (cursor-acp--session-ask-question-request-ids sess)))
            (when (and tool-call-id (hash-table-p cache))
              (puthash tool-call-id outcome cache))
            (dolist (rid ids)
              (when (integerp rid)
                (cursor-acp--rpc-respond sess rid `((outcome . ,outcome)))))
            (setf (cursor-acp--session-ask-question-tool-call-id sess) nil)
            (setf (cursor-acp--session-ask-question-request-id sess) nil)
            (setf (cursor-acp--session-ask-question-request-ids sess) nil)
            (setf (cursor-acp--session-ask-question-request-params sess) nil)
            (cursor-acp--set-awaiting-ask-question sess nil)
            (cursor-acp--render-info sess))
        (quit
         (cursor-acp--chat-insert sess "[question prompt cancelled - use C-c C-b to retry]\n" t)
         (cursor-acp--render-info sess)))))))

(defun cursor-acp--cancel-pending-ask-question (sess)
  (when-let ((ids (or (cursor-acp--session-ask-question-request-ids sess)
                      (when-let ((rid (cursor-acp--session-ask-question-request-id sess)))
                        (list rid)))))
    (dolist (rid ids)
      (when (integerp rid)
        (cursor-acp--rpc-respond sess rid '((outcome . ((outcome . "cancelled")))))))
    (setf (cursor-acp--session-ask-question-tool-call-id sess) nil)
    (setf (cursor-acp--session-ask-question-request-id sess) nil)
    (setf (cursor-acp--session-ask-question-request-ids sess) nil)
    (setf (cursor-acp--session-ask-question-request-params sess) nil)
    (cursor-acp--set-awaiting-ask-question sess nil)
    (cursor-acp--chat-insert sess "[ask question cancelled]\n" t)
    t))

(defun cursor-acp--reprompt-pending-ask-question (sess)
  (let ((rid (cursor-acp--session-ask-question-request-id sess))
        (params (cursor-acp--session-ask-question-request-params sess)))
    (unless (and (integerp rid) (hash-table-p params))
      (user-error "No pending ask question request"))
    (let* ((tool-call-id (cursor-acp--session-ask-question-tool-call-id sess))
           (cache (cursor-acp--session-ask-question-response-cache sess))
           (ids (or (cursor-acp--session-ask-question-request-ids sess) (list rid)))
           (outcome (cursor-acp--prompt-ask-question-decision sess params)))
      (when (and tool-call-id (hash-table-p cache))
        (puthash tool-call-id outcome cache))
      (dolist (rid0 ids)
        (when (integerp rid0)
          (cursor-acp--rpc-respond sess rid0 `((outcome . ,outcome)))))
      (setf (cursor-acp--session-ask-question-tool-call-id sess) nil)
      (setf (cursor-acp--session-ask-question-request-id sess) nil)
      (setf (cursor-acp--session-ask-question-request-ids sess) nil)
      (setf (cursor-acp--session-ask-question-request-params sess) nil)
      (cursor-acp--set-awaiting-ask-question sess nil)
      (cursor-acp--render-info sess))))

(defun cursor-acp--file-uri (path)
  (concat "file://" (expand-file-name path)))

(defun cursor-acp--write-plan-file (sess params)
  (let* ((name (or (and (hash-table-p params) (cursor-acp--ht-get params "name")) "plan"))
         (overview (and (hash-table-p params) (cursor-acp--ht-get params "overview")))
         (plan (and (hash-table-p params) (cursor-acp--ht-get params "plan")))
         (todos (and (hash-table-p params) (cursor-acp--ht-get params "todos")))
         (path (cursor-acp--plan-path sess (if (stringp name) name "plan")))
         (dir (file-name-directory path)))
    (when (and (stringp dir) (not (file-directory-p dir)))
      (make-directory dir t))
    (with-temp-buffer
      (let ((nm (string-trim (format "%s" (or name "plan")))))
        (insert "# " (if (string-empty-p nm) "Plan" nm) "\n\n"))
      (when (and (stringp overview) (not (string-empty-p (string-trim overview))))
        (insert (string-trim overview) "\n\n"))
      (when (stringp plan)
        (insert (string-trim-right plan) "\n\n"))
      (insert (cursor-acp--plan-todos-section-text todos))
      (write-region (point-min) (point-max) path nil 'quiet))
    path))

(defun cursor-acp--preview-plan-file (_sess path)
  (let ((buf (find-file-noselect path)))
    (with-current-buffer buf
      (if (fboundp 'gfm-mode)
          (gfm-mode)
        (markdown-mode))
      (cursor-acp--plan-buffer-after-insert-markdown-view))
    (display-buffer buf)
    buf))

(defun cursor-acp--handle-cursor-create-plan (sess id params)
  (let* ((todos (cursor-acp--normalize-items (and (hash-table-p params) (cursor-acp--ht-get params "todos"))))
         (path (cursor-acp--write-plan-file sess params))
         (_todos (progn
                   (setf (cursor-acp--session-create-plan-path sess) path)
                   (setf (cursor-acp--session-plan-entries sess) todos)
                   (cursor-acp--render-plan-entries sess todos)))
         (_buf (cursor-acp--preview-plan-file sess path))
         (choice (completing-read "Plan: " '("accept" "reject" "cancel") nil t nil nil "accept")))
    (cond
     ((string-equal choice "accept")
      (cursor-acp--rpc-respond
       sess id
       `((outcome . ((outcome . "accepted")
                     (planUri . ,(cursor-acp--file-uri path)))))))
     ((string-equal choice "reject")
      (setf (cursor-acp--session-create-plan-path sess) nil)
      (setf (cursor-acp--session-plan-entries sess) nil)
      (cursor-acp--render-plan-entries sess nil)
      (let* ((reason (read-string "Reject reason (optional): "))
             (reason0 (and (stringp reason) (string-trim reason)))
             (out (if (and (stringp reason0) (not (string-empty-p reason0)))
                      `((outcome . "rejected") (reason . ,reason0))
                    '((outcome . "rejected")))))
        (cursor-acp--rpc-respond sess id `((outcome . ,out)))))
     (t
      (setf (cursor-acp--session-create-plan-path sess) nil)
      (setf (cursor-acp--session-plan-entries sess) nil)
      (cursor-acp--render-plan-entries sess nil)
      (cursor-acp--rpc-respond sess id '((outcome . ((outcome . "cancelled")))))))))

(defun cursor-acp--reprompt-pending-create-plan (sess)
  (let ((rid (cursor-acp--session-create-plan-request-id sess))
        (params (cursor-acp--session-create-plan-request-params sess)))
    (unless (and (integerp rid) (hash-table-p params))
      (user-error "No pending create plan request"))
    (condition-case err
        (progn
          (cursor-acp--handle-cursor-create-plan sess rid params)
          (setf (cursor-acp--session-awaiting-create-plan sess) nil)
          (setf (cursor-acp--session-create-plan-request-id sess) nil)
          (setf (cursor-acp--session-create-plan-request-params sess) nil)
          (cursor-acp--render-info sess))
      (error
       (cursor-acp--rpc-respond-error sess rid -32602 (error-message-string err))
       (setf (cursor-acp--session-awaiting-create-plan sess) nil)
       (setf (cursor-acp--session-create-plan-request-id sess) nil)
       (setf (cursor-acp--session-create-plan-request-params sess) nil)
       (cursor-acp--render-info sess)))))

(defun cursor-acp--handle-message (sess msg)
  (let ((id (gethash "id" msg nil))
        (method (gethash "method" msg nil)))
    (cond
     ;; Agent -> client filesystem requests
     ((and id method (string-equal method "fs/read_text_file"))
      (condition-case err
          (cursor-acp--rpc-respond sess id (cursor-acp--fs-read-text-file sess (gethash "params" msg nil)))
        (error
         (cursor-acp--rpc-respond-error sess id -32602 (error-message-string err)))))
     ((and id method (string-equal method "fs/write_text_file"))
      (condition-case err
          (let* ((params (gethash "params" msg nil))
                 (path (and (hash-table-p params) (gethash "path" params nil))))
            (cursor-acp--fs-write-text-file sess params)
            (when (stringp path)
              (cursor-acp--render-write-text-file sess path))
            (cursor-acp--rpc-respond sess id nil))
        (error
         (cursor-acp--rpc-respond-error sess id -32602 (error-message-string err)))))

     ;; Agent -> client terminal requests
     ((and id method (string-equal method "terminal/create"))
      (condition-case err
          (cursor-acp--rpc-respond sess id (cursor-acp--terminal-create sess (gethash "params" msg nil)))
        (error
         (cursor-acp--rpc-respond-error sess id -32602 (error-message-string err)))))
     ((and id method (string-equal method "terminal/output"))
      (condition-case err
          (cursor-acp--rpc-respond sess id (cursor-acp--terminal-output-rpc sess (gethash "params" msg nil)))
        (error
         (cursor-acp--rpc-respond-error sess id -32602 (error-message-string err)))))
     ((and id method (string-equal method "terminal/wait_for_exit"))
      (condition-case err
          (let ((res (cursor-acp--terminal-wait-for-exit sess id (gethash "params" msg nil))))
            (unless (eq res :cursor-acp--deferred)
              (cursor-acp--rpc-respond sess id res)))
        (error
         (cursor-acp--rpc-respond-error sess id -32602 (error-message-string err)))))
     ((and id method (string-equal method "terminal/kill"))
      (condition-case err
          (progn
            (cursor-acp--terminal-kill sess (gethash "params" msg nil))
            (cursor-acp--rpc-respond sess id nil))
        (error
         (cursor-acp--rpc-respond-error sess id -32602 (error-message-string err)))))
     ((and id method (string-equal method "terminal/release"))
      (condition-case err
          (progn
            (cursor-acp--terminal-release sess (gethash "params" msg nil))
            (cursor-acp--rpc-respond sess id nil))
        (error
         (cursor-acp--rpc-respond-error sess id -32602 (error-message-string err)))))

     ;; Agent -> client permission request
     ((and id method (string-equal method "session/request_permission"))
      (let* ((params (gethash "params" msg nil))
             (sid (and (hash-table-p params) (gethash "sessionId" params nil)))
             (target (if (stringp sid) (cursor-acp--ensure-session-by-id sid) sess)))
        (cursor-acp--handle-permission-request target id params)))

     ((and id method (string-equal method "cursor/update_todos"))
      (condition-case err
          (let* ((params (gethash "params" msg nil))
                 (sid (and (hash-table-p params) (cursor-acp--ht-get params "sessionId")))
                 (target (if (stringp sid)
                             (cursor-acp--ensure-session-by-id sid)
                           sess))
                 (todos (and (hash-table-p params) (cursor-acp--ht-get params "todos")))
                 (mergep (cursor-acp--plan-update-todos-merge-p params)))
            (unless (cursor-acp--valid-session-p target)
              (error "No target session for update_todos"))
            (let ((merged (cursor-acp--merge-plan-todo-entries
                           (cursor-acp--session-plan-entries target)
                           todos
                           mergep)))
              (setf (cursor-acp--session-plan-entries target) merged)
              (cursor-acp--render-plan-entries target merged)
              (cursor-acp--plan-file-sync-todos-section target))
            (cursor-acp--rpc-respond target id nil))
        (error
         (cursor-acp--rpc-respond-error sess id -32602 (error-message-string err)))))

     ;; Cursor extension: blocking plan approval
     ((and id method (string-equal method "cursor/create_plan"))
      (condition-case err
          (let ((params (gethash "params" msg nil)))
            (setf (cursor-acp--session-awaiting-create-plan sess) t)
            (setf (cursor-acp--session-create-plan-request-id sess) id)
            (setf (cursor-acp--session-create-plan-request-params sess) params)
            (cursor-acp--handle-cursor-create-plan sess id params)
            (setf (cursor-acp--session-awaiting-create-plan sess) nil)
            (setf (cursor-acp--session-create-plan-request-id sess) nil)
            (setf (cursor-acp--session-create-plan-request-params sess) nil))
        (error
         (cursor-acp--rpc-respond-error sess id -32602 (error-message-string err)))))

     ((and id method (string-equal method "cursor/ask_question"))
      (let ((params (gethash "params" msg nil)))
        (cursor-acp--handle-ask-question-request sess id params)))

     ;; Response to a prior request
     (id
      (let* ((conn (cursor-acp--ensure-conn))
             (pending (cursor-acp--conn-pending conn))
             (state (gethash id pending)))
        (when state
          (if-let ((err (gethash "error" msg nil)))
              (progn
                (plist-put state :done t)
                (plist-put state :error err))
            (plist-put state :done t)
            (plist-put state :result (gethash "result" msg nil))))
        (let* ((sid (and state (plist-get state :session-id)))
               (target (if (stringp sid) (cursor-acp--session-by-id sid) sess))
               (res (gethash "result" msg nil))
               (sr (and (hash-table-p res) (gethash "stopReason" res nil))))
          (when (and (cursor-acp--valid-session-p target)
                     (cursor-acp--terminal-stop-reason-p sr))
            (cursor-acp--set-busy target nil)
            (cursor-acp--assistant-end-turn target)
            (cursor-acp--render-info target)))))

     ;; Agent -> client streaming updates
     ((and method (string-equal method "session/update"))
      (let* ((params (gethash "params" msg nil))
             (sid (and (hash-table-p params) (gethash "sessionId" params nil)))
             (target (if (stringp sid) (cursor-acp--ensure-session-by-id sid) sess))
             (update (and (hash-table-p params) (gethash "update" params nil))))
        (when (hash-table-p update)
          (cursor-acp--maybe-cache-commands-from-update target update)
          (cursor-acp--maybe-cache-config-options-from-update target update)
          (when-let ((mode (or (cursor-acp--ht-get update "mode")
                               (cursor-acp--ht-get update "modeId")
                               (cursor-acp--ht-get update "currentModeId"))))
            (when (stringp mode)
              (setf (cursor-acp--session-current-mode target) mode)))
          (when-let ((model (or (cursor-acp--ht-get update "model")
                                (cursor-acp--ht-get update "modelId")
                                (cursor-acp--ht-get update "currentModelId"))))
            (when (stringp model)
              (setf (cursor-acp--session-current-model target) model)))
          (cursor-acp--chat-refresh-header target)
          (let ((kind (cursor-acp--ht-get update "sessionUpdate")))
            (cond
             ((string-equal kind "session_info_update")
              (let ((title (cursor-acp--ht-get update "title")))
                (when (or (null title) (stringp title))
                  (setf (cursor-acp--session-title target) (or title "Untitled"))
                  (cursor-acp--rename-session-buffers target))))
             ((string-equal kind "plan")
              (let ((entries (cursor-acp--normalize-items (cursor-acp--ht-get update "entries"))))
                (setf (cursor-acp--session-plan-entries target) entries)
                (cursor-acp--render-plan-entries target entries)
                (cursor-acp--plan-file-sync-todos-section target)))
             ((string-equal kind "agent_message_chunk")
              (let* ((content (cursor-acp--ht-get update "content"))
                     (txt (and (hash-table-p content) (cursor-acp--ht-get content "text"))))
                (when (stringp txt)
                  (cursor-acp--chat-open-assistant target)
                  (cursor-acp--assistant-append target txt))))
             ((string-equal kind "tool_call")
              (let* ((tool-call-id (cursor-acp--ht-get update "toolCallId"))
                     (title (cursor-acp--ht-get update "title"))
                     (tc-kind (cursor-acp--ht-get update "kind"))
                     (status (cursor-acp--ht-get update "status"))
                     (ht (cursor-acp--session-tool-calls target))
                     (id0 (and (stringp tool-call-id) (string-trim tool-call-id))))
                (when (and (hash-table-p ht) (stringp id0) (not (string-empty-p id0)))
                  (puthash id0
                           `((title . ,title)
                             (kind . ,tc-kind)
                             (status . ,status))
                           ht))))
             ((and (string-equal kind "tool_call_update")
                   (stringp (cursor-acp--ht-get update "status")))
              (let* ((tool-call-id (cursor-acp--ht-get update "toolCallId"))
                     (status (cursor-acp--ht-get update "status"))
                     (content (cursor-acp--ht-get update "content"))
                     (raw (cursor-acp--ht-get update "rawOutput"))
                     (ht (cursor-acp--session-tool-calls target))
                     (id0 (and (stringp tool-call-id) (string-trim tool-call-id)))
                     (cached (and (hash-table-p ht) id0 (gethash id0 ht nil)))
                     (title (and (listp cached) (cdr (assoc 'title cached))))
                     (tc-kind (and (listp cached) (cdr (assoc 'kind cached)))))
                (when (and (hash-table-p ht) (stringp id0) (not (string-empty-p id0)))
                  (puthash id0
                           `((title . ,title)
                             (kind . ,tc-kind)
                             (status . ,status)
                             (rawOutputPresent . ,(and raw t)))
                           ht))
                (when (string-equal status "completed")
                  (when content
                    (cursor-acp--maybe-render-diffs-from target content))
                  (when raw
                    (cursor-acp--maybe-render-diffs-from target raw)
                    (cursor-acp--render-tool-call-completed
                     target tool-call-id title raw tc-kind)))))))
          (cursor-acp--render-info target)))))))

(defun cursor-acp--process-filter (proc chunk)
  (let* ((sess (cursor-acp--ensure-session))
         (conn (cursor-acp--ensure-conn)))
    (when (and sess (eq proc (cursor-acp--conn-process conn)))
      (setf (cursor-acp--conn-stdout-acc conn)
            (concat (or (cursor-acp--conn-stdout-acc conn) "") chunk))
      (let ((acc (cursor-acp--conn-stdout-acc conn))
            (start 0)
            (len (length (cursor-acp--conn-stdout-acc conn))))
        (while (and (< start len) (string-match "\n" acc start))
          (let* ((end (match-beginning 0))
                 (line (string-trim (substring acc start end))))
            (setq start (1+ end))
            (unless (string-empty-p line)
              (cursor-acp--log sess "<--" line)
              (condition-case err
                  (let ((msg (json-parse-string line :object-type 'hash-table :array-type 'list)))
                    (condition-case herr
                        (cursor-acp--handle-message sess msg)
                      (error
                       (cursor-acp--log sess "<--handler-error" (format "%S\n%s" herr line)))))
                (error
                 (cursor-acp--log sess "<--parse-error" (format "%S\n%s" err line)))))))
        (setf (cursor-acp--conn-stdout-acc conn) (substring acc start))))))

(defun cursor-acp--process-sentinel (proc event)
  (let* ((sess (cursor-acp--active-session))
         (conn (cursor-acp--ensure-conn)))
    (when (and sess (eq proc (cursor-acp--conn-process conn)))
      (cursor-acp--log sess "process" (string-trim event))
      (cursor-acp--assistant-flush-frag sess)
      (cursor-acp--set-busy sess nil)
      (setf (cursor-acp--conn-process conn) nil)
      (ignore-errors (cursor-acp--chat-refresh-header sess))
      (ignore-errors (cursor-acp--render-info sess)))))

(defun cursor-acp--ensure-process (sess)
  (let* ((conn (cursor-acp--ensure-conn))
         (proc (cursor-acp--conn-process conn)))
    (if (and proc (process-live-p proc))
        proc
      (let* ((log (cursor-acp--session-log-buffer sess))
             (proc (make-process
                    :name "cursor-acp"
                    :buffer nil
                    :command (cons cursor-acp-agent-command cursor-acp-agent-args)
                    :coding 'utf-8-unix
                    :connection-type 'pipe
                    :noquery t
                    :filter #'cursor-acp--process-filter
                    :sentinel #'cursor-acp--process-sentinel
                    :stderr log)))
        (setf (cursor-acp--conn-process conn) proc)
        (cursor-acp--log sess "process" "spawned")
        (cursor-acp--chat-refresh-header sess)
        proc))))

(defun cursor-acp--handshake (sess)
  (let ((init-res
         (cursor-acp--rpc-call
          sess "initialize"
          `((protocolVersion . 1)
            (clientCapabilities . ((fs . ((readTextFile . t)
                                          (writeTextFile . t)))
                                   (terminal . t)))
            (clientInfo . ((name . "emacs-cursor-acp") (version . "0.0.1")))))))
    (when (hash-table-p init-res)
      (setf (cursor-acp--conn-init-result (cursor-acp--ensure-conn)) init-res)
      (cursor-acp--cache-capabilities sess init-res)))
  (cursor-acp--rpc-call sess "authenticate" '((methodId . "cursor_login")))
  (let* ((cwd (expand-file-name default-directory))
         (res (cursor-acp--rpc-call
               sess "session/new"
               `((cwd . ,cwd)
                 (mcpServers . [])))))
    (when (hash-table-p res)
      (let* ((sid (gethash "sessionId" res nil))
             (new (cursor-acp--ensure-session-by-id sid nil cwd)))
        (cursor-acp--set-active-session-id sid)
        (cursor-acp--cache-session-new new res)
        (cursor-acp--render-info new)))))

(defun cursor-acp--ensure-connected-session (sess)
  (unless (and (process-live-p (cursor-acp--conn-process (cursor-acp--ensure-conn)))
               (cursor-acp--session-session-id sess))
    (cursor-acp-start))
  (cursor-acp--session-session-id (or (cursor-acp--active-session) sess)))

(defun cursor-acp--set-config-option (sess config-id value)
  (let ((sid (cursor-acp--ensure-connected-session sess)))
    (let ((res
           (cursor-acp--rpc-call
            sess "session/set_config_option"
            `((sessionId . ,sid) (configId . ,config-id) (value . ,value)))))
      (when (hash-table-p res)
        (cursor-acp--cache-config-options sess (cursor-acp--ht-get res "configOptions"))
        (cursor-acp--sync-current-from-config-options sess))
      (cursor-acp--chat-refresh-header sess)
      (cursor-acp--render-info sess))))

(defun cursor-acp--plan-preview-refresh (path)
  (when (stringp path)
    (let ((abs (expand-file-name path)))
      (dolist (buf (buffer-list))
        (let ((f (buffer-file-name buf)))
          (when (and (buffer-live-p buf) (stringp f)
                     (string-equal (expand-file-name f) abs))
            (with-current-buffer buf
              (ignore-errors (revert-buffer t t t)))))))))

(defun cursor-acp--plan-file-sync-todos-section (sess)
  (let ((path (cursor-acp--session-create-plan-path sess))
        (block (cursor-acp--plan-todos-section-text (cursor-acp--session-plan-entries sess))))
    (when (and (stringp path) (file-exists-p path) (stringp block))
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (if (re-search-forward "^## Todos[ \t]*\n" nil t)
            (progn
              (delete-region (match-beginning 0) (point-max))
              (goto-char (match-beginning 0))
              (insert block))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert block))
        (write-region (point-min) (point-max) path nil 'quiet))
      (cursor-acp--plan-preview-refresh path))))

(defun cursor-acp--cap-session-list-p ()
  (let* ((init (cursor-acp--conn-init-result (cursor-acp--ensure-conn)))
         (agent-cap (cursor-acp--ht-get init "agentCapabilities"))
         (sess-cap (and (hash-table-p agent-cap) (cursor-acp--ht-get agent-cap "sessionCapabilities")))
         (list-cap (and (hash-table-p sess-cap) (cursor-acp--ht-get sess-cap "list"))))
    (and list-cap t)))

(defun cursor-acp--cap-load-session-p ()
  (let* ((init (cursor-acp--conn-init-result (cursor-acp--ensure-conn)))
         (agent-cap (cursor-acp--ht-get init "agentCapabilities"))
         (load (and (hash-table-p agent-cap) (cursor-acp--ht-get agent-cap "loadSession"))))
    (and load t)))

(defun cursor-acp--cap-session-resume-p ()
  (let* ((init (cursor-acp--conn-init-result (cursor-acp--ensure-conn)))
         (agent-cap (cursor-acp--ht-get init "agentCapabilities"))
         (sess-cap (and (hash-table-p agent-cap) (cursor-acp--ht-get agent-cap "sessionCapabilities")))
         (resume-cap (and (hash-table-p sess-cap) (cursor-acp--ht-get sess-cap "resume"))))
    (and resume-cap t)))

(defun cursor-acp--rpc-session-list (sess &optional cursor)
  (unless (cursor-acp--cap-session-list-p)
    (user-error "Agent does not advertise sessionCapabilities.list"))
  (cursor-acp--rpc-call
   sess "session/list"
   `((cwd . ,(expand-file-name default-directory))
     ,@(when (and (stringp cursor) (not (string-empty-p cursor)))
         `((cursor . ,cursor))))))

(defun cursor-acp--rpc-session-load (sess session-id)
  (unless (cursor-acp--cap-load-session-p)
    (user-error "Agent does not advertise loadSession"))
  (cursor-acp--rpc-send-async
   sess "session/load"
   `((sessionId . ,session-id)
     (cwd . ,(expand-file-name default-directory))
     (mcpServers . []))
   session-id))

(defun cursor-acp--rpc-session-resume (sess session-id)
  (unless (cursor-acp--cap-session-resume-p)
    (user-error "Agent does not advertise sessionCapabilities.resume"))
  (cursor-acp--rpc-call
   sess "session/resume"
   `((sessionId . ,session-id)
     (cwd . ,(expand-file-name default-directory))
     (mcpServers . []))))

(provide 'cursor-acp-transport)
;;; cursor-acp-transport.el ends here
