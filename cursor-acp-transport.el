;;; cursor-acp-transport.el --- Cursor ACP transport -*- lexical-binding: t; -*-

(require 'cursor-acp-core)
(require 'cursor-acp-ui)

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
    (let ((parent (file-name-directory (expand-file-name path))))
      (when (and (stringp parent) (not (file-directory-p parent)))
        (make-directory parent t)))
    (with-temp-buffer
      (insert content)
      (write-region (point-min) (point-max) path nil 'quiet))
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

(defun cursor-acp--handle-permission-request (sess id params)
  (setf (cursor-acp--session-permission-request-id sess) id)
  (setf (cursor-acp--session-permission-request-params sess) params)
  (cursor-acp--set-awaiting-permission sess t)
  (cursor-acp--assistant-flush-frag sess)
  (condition-case _
      (let ((outcome (cursor-acp--prompt-permission-decision sess params)))
        (cursor-acp--rpc-respond sess id `((outcome . ,outcome)))
        (setf (cursor-acp--session-permission-request-id sess) nil)
        (setf (cursor-acp--session-permission-request-params sess) nil)
        (cursor-acp--set-awaiting-permission sess nil)
        (cursor-acp--render-info sess))
    (quit
     (cursor-acp--chat-insert sess "[permission prompt cancelled - use C-c C-a to retry]\n" t)
     (cursor-acp--render-info sess))))

(defun cursor-acp--cancel-pending-permission (sess)
  (when-let ((rid (cursor-acp--session-permission-request-id sess)))
    (cursor-acp--rpc-respond sess rid '((outcome . ((outcome . "cancelled")))))
    (setf (cursor-acp--session-permission-request-id sess) nil)
    (setf (cursor-acp--session-permission-request-params sess) nil)
    (cursor-acp--set-awaiting-permission sess nil)
    (cursor-acp--chat-insert sess "[permission cancelled]\n" t)
    t))

(defun cursor-acp--reprompt-pending-permission (sess)
  (let ((rid (cursor-acp--session-permission-request-id sess))
        (params (cursor-acp--session-permission-request-params sess)))
    (unless (and rid (hash-table-p params))
      (user-error "No pending permission request"))
    (let ((outcome (cursor-acp--prompt-permission-decision sess params)))
      (cursor-acp--rpc-respond sess rid `((outcome . ,outcome)))
      (setf (cursor-acp--session-permission-request-id sess) nil)
      (setf (cursor-acp--session-permission-request-params sess) nil)
      (cursor-acp--set-awaiting-permission sess nil)
      (cursor-acp--render-info sess))))

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
          (cursor-acp--rpc-respond sess id (cursor-acp--fs-write-text-file sess (gethash "params" msg nil)))
        (error
         (cursor-acp--rpc-respond-error sess id -32602 (error-message-string err)))))

     ;; Agent -> client permission request
     ((and id method (string-equal method "session/request_permission"))
      (let* ((params (gethash "params" msg nil))
             (sid (and (hash-table-p params) (gethash "sessionId" params nil)))
             (target (if (stringp sid) (cursor-acp--ensure-session-by-id sid) sess)))
        (cursor-acp--handle-permission-request target id params)))

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
          (with-current-buffer (cursor-acp--session-chat-buffer target)
            (setq-local header-line-format (cursor-acp--status-line target)))
          (let ((kind (cursor-acp--ht-get update "sessionUpdate")))
            (cond
             ((string-equal kind "session_info_update")
              (let ((title (cursor-acp--ht-get update "title")))
                (when (or (null title) (stringp title))
                  (setf (cursor-acp--session-title target) (or title "Untitled"))
                  (cursor-acp--rename-session-buffers target))))
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
                     (raw (cursor-acp--ht-get update "rawOutput"))
                     (ht (cursor-acp--session-tool-calls target))
                     (id0 (and (stringp tool-call-id) (string-trim tool-call-id)))
                     (cached (and (hash-table-p ht) id0 (gethash id0 ht nil)))
                     (title (and (listp cached) (cdr (assoc 'title cached)))))
                (when (and (hash-table-p ht) (stringp id0) (not (string-empty-p id0)))
                  (puthash id0
                           `((title . ,title)
                             (status . ,status)
                             (rawOutputPresent . ,(and raw t)))
                           ht))
                (when (and (string-equal status "completed") raw)
                  (cursor-acp--render-tool-call-completed
                   target tool-call-id title raw))))))
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
      (ignore-errors
        (with-current-buffer (cursor-acp--session-chat-buffer sess)
          (setq-local header-line-format (cursor-acp--status-line sess))))
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
        (with-current-buffer (cursor-acp--session-chat-buffer sess)
          (setq-local header-line-format (cursor-acp--status-line sess)))
        proc))))

(defun cursor-acp--handshake (sess)
  (let ((init-res
         (cursor-acp--rpc-call
          sess "initialize"
          `((protocolVersion . 1)
            (clientCapabilities . ((fs . ((readTextFile . t)
                                          (writeTextFile . t)))
                                   (terminal . :json-false)))
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
      (with-current-buffer (cursor-acp--session-chat-buffer sess)
        (setq-local header-line-format (cursor-acp--status-line sess)))
      (cursor-acp--render-info sess))))

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
