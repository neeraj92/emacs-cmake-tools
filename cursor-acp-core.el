;;; cursor-acp-core.el --- Cursor ACP core -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'outline)
(require 'subr-x)

(defgroup cursor-acp nil
  "Cursor ACP client."
  :group 'external
  :prefix "cursor-acp-")

(defcustom cursor-acp-agent-command "agent"
  "Command used to start ACP."
  :type 'string
  :group 'cursor-acp)

(defcustom cursor-acp-agent-args '("acp")
  "Arguments passed to `cursor-acp-agent-command`."
  :type '(repeat string)
  :group 'cursor-acp)

(defface cursor-acp-header-face
  '((t :inherit bold))
  "Face for the header."
  :group 'cursor-acp)

(defface cursor-acp-status-ok-face
  '((t :inherit success))
  "Face for ok status."
  :group 'cursor-acp)

(defface cursor-acp-status-bad-face
  '((t :inherit error))
  "Face for bad status."
  :group 'cursor-acp)

(defface cursor-acp-user-prefix-face
  '((t :inherit error))
  "Face for user prompt prefix in chat buffer."
  :group 'cursor-acp)

(defface cursor-acp-agent-prefix-face
  '((t :inherit success))
  "Face for agent prompt prefix in chat buffer."
  :group 'cursor-acp)

(defface cursor-acp-agent-text-face
  '((t :inherit default :foreground "green"))
  "Face for agent response text in chat buffer."
  :group 'cursor-acp)

(cl-defstruct (cursor-acp--conn (:constructor cursor-acp--make-conn))
  process
  stdout-acc
  pending
  next-id
  init-result)

(cl-defstruct (cursor-acp--session (:constructor cursor-acp--make-session))
  session-id
  title
  cwd
  current-mode
  current-model
  config-options
  available-modes
  available-models
  available-commands
  ui-expanded
  tool-calls
  plan-entries
  plan-buffer
  awaiting-create-plan
  create-plan-request-id
  create-plan-request-params
  create-plan-path
  busy
  spinner-idx
  spinner-timer
  awaiting-permission
  permission-request-id
  permission-request-ids
  permission-tool-call-id
  permission-request-params
  permission-response-cache
  awaiting-ask-question
  ask-question-request-id
  ask-question-request-ids
  ask-question-tool-call-id
  ask-question-request-params
  ask-question-response-cache
  assistant-frag
  draft-input
  main-buffer
  chat-buffer
  input-buffer
  info-buffer
  log-buffer)

(defvar cursor-acp--conn nil)
(defvar cursor-acp--sessions nil
  "Hash table mapping sessionId -> `cursor-acp--session'.")
(defvar cursor-acp--active-session-id nil)

(defconst cursor-acp--info-buffer-name "*cursor-acp-info*")
(defconst cursor-acp--log-buffer-name "*cursor-acp-log*")

(defconst cursor-acp--spinner-frames ["|" "/" "-" "\\"]
  "ASCII spinner frames used in the header line while busy.")

(defconst cursor-acp--spinner-interval 0.15
  "Spinner update interval in seconds.")

(defcustom cursor-acp-chat-flush-words 25
  "Flush buffered agent text after this many words.

This avoids inserting one token/word at a time when the server streams tiny chunks."
  :type 'integer
  :group 'cursor-acp)

(defcustom cursor-acp-markdown-hide-markup t
  "If non-nil, hide Markdown markup in chat and in plan/todo views.

Chat uses `cursor-acp--hide-markup-region' overlays. Plan file and side
todo buffers use `markdown-mode' / `gfm-mode' and `markdown-hide-markup'
when the installed markdown-mode supports it."
  :type 'boolean
  :group 'cursor-acp)

(defcustom cursor-acp-pane-width 80
  "Width in columns for the Cursor ACP right-side pane."
  :type 'integer
  :group 'cursor-acp)

(defcustom cursor-acp-review-root-dir
  (let ((xdg (getenv "XDG_CONFIG_HOME")))
    (expand-file-name
     "emacs-cursor-acp/diffs/"
     (if (and (stringp xdg) (not (string-empty-p (string-trim xdg))))
         (file-name-as-directory (expand-file-name xdg))
       (expand-file-name "~/.config/"))))
  "Root directory for Cursor ACP review seeds.

Seeds are stored per project (workspace root) under this directory."
  :type 'directory
  :group 'cursor-acp)

(defcustom cursor-acp-review-skip-relative-prefixes
  '(".cursor/plans")
  "Path prefixes (relative to `cursor-acp--workspace-root') to skip for reviews.

When a changed file path matches a prefix, no review seed is created and it is
omitted from `cursor-acp-review' completion. Matching is prefix-based after
normalizing slashes; a trailing slash on an entry is optional."
  :type '(repeat string)
  :group 'cursor-acp)

(defcustom cursor-acp-terminal-root-dir
  (let ((xdg (getenv "XDG_CONFIG_HOME")))
    (expand-file-name
     "emacs-cursor-acp/terminals/"
     (if (and (stringp xdg) (not (string-empty-p (string-trim xdg))))
         (file-name-as-directory (expand-file-name xdg))
       (expand-file-name "~/.config/"))))
  "Root directory for Cursor ACP terminal output logs.

Terminal logs are stored per project (workspace root) under this directory."
  :type 'directory
  :group 'cursor-acp)

(defcustom cursor-acp-session-transcript-root-dir
  (let ((xdg (getenv "XDG_CONFIG_HOME")))
    (expand-file-name
     "emacs-cursor-acp/sessions/"
     (if (and (stringp xdg) (not (string-empty-p (string-trim xdg))))
         (file-name-as-directory (expand-file-name xdg))
       (expand-file-name "~/.config/"))))
  "Root directory for persisted ACP chat transcripts.

Each workspace has a subdirectory; each session id has a folder with a file
named \"transcript\". This tree is separate from `cursor-acp-review-root-dir'."
  :type 'directory
  :group 'cursor-acp)

(defun cursor-acp--review--sanitize (s)
  (replace-regexp-in-string "[^A-Za-z0-9_.-]" "_" (or s "")))

(defun cursor-acp--review-project-dir (sess)
  (let* ((root (cursor-acp--workspace-root sess))
         (key (cursor-acp--review--sanitize (directory-file-name (expand-file-name root)))))
    (file-name-as-directory (expand-file-name key cursor-acp-review-root-dir))))

(defun cursor-acp--terminal-project-dir (sess)
  (let* ((root (cursor-acp--workspace-root sess))
         (key (cursor-acp--review--sanitize (directory-file-name (expand-file-name root)))))
    (file-name-as-directory (expand-file-name key cursor-acp-terminal-root-dir))))

(defun cursor-acp--terminal-log-file (sess terminal-id)
  (let* ((proj (cursor-acp--terminal-project-dir sess))
         (name (cursor-acp--review--sanitize (or terminal-id "term_unknown"))))
    (expand-file-name (format "%s.log" name) proj)))

(defun cursor-acp--session-transcript-workspace-key (sess)
  (cursor-acp--review--sanitize
   (directory-file-name (expand-file-name (cursor-acp--workspace-root sess)))))

(defun cursor-acp--session-transcript-file (sess)
  (let ((sid (and (cursor-acp--valid-session-p sess)
                  (cursor-acp--session-session-id sess))))
    (when (and (stringp sid) (not (string-empty-p sid)))
      (let* ((root (file-name-as-directory
                    (expand-file-name cursor-acp-session-transcript-root-dir)))
             (wk (cursor-acp--session-transcript-workspace-key sess))
             (dir (file-name-as-directory
                   (expand-file-name (cursor-acp--review--sanitize sid)
                                     (expand-file-name wk root)))))
        (expand-file-name "transcript" dir)))))

(defun cursor-acp--session-transcript-save (sess)
  (when (and (cursor-acp--valid-session-p sess)
             (let ((sid (cursor-acp--session-session-id sess)))
               (and (stringp sid) (not (string-empty-p sid)))))
    (when-let ((path (cursor-acp--session-transcript-file sess))
               (buf (cursor-acp--session-chat-buffer sess)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((text (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
            (unless (string-empty-p text)
              (let ((dir (file-name-directory path)))
                (when (and (stringp dir) (not (file-directory-p dir)))
                  (make-directory dir t))
                (write-region text nil path nil 'quiet)))))))))

(defun cursor-acp--session-transcript-load-into-chat (sess)
  (when-let ((path (cursor-acp--session-transcript-file sess))
             (buf (cursor-acp--session-chat-buffer sess)))
    (when (and (file-readable-p path) (buffer-live-p buf))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (insert-file-contents path)))
      (with-current-buffer buf
        (setq-local cursor-acp--assistant-open nil)
        (setq-local cursor-acp--assistant-start nil))
      t)))

(defun cursor-acp--review-seed-path (sess abs-path)
  (let* ((root (cursor-acp--workspace-root sess))
         (rel (file-relative-name (expand-file-name abs-path) (file-name-as-directory (expand-file-name root))))
         (proj (cursor-acp--review-project-dir sess)))
    (expand-file-name rel proj)))

(defun cursor-acp--review-seed-exists-p (sess abs-path)
  (file-exists-p (cursor-acp--review-seed-path sess abs-path)))

(defun cursor-acp--review-ensure-seed (sess abs-path seed-text)
  (let* ((seed (cursor-acp--review-seed-path sess abs-path))
         (dir (file-name-directory seed)))
    (unless (file-exists-p seed)
      (when (and (stringp dir) (not (file-directory-p dir)))
        (make-directory dir t))
      (with-temp-buffer
        (insert (or seed-text ""))
        (write-region (point-min) (point-max) seed nil 'quiet)))
    seed))

(defun cursor-acp--review-delete-seed (sess abs-path)
  (let ((seed (cursor-acp--review-seed-path sess abs-path)))
    (when (file-exists-p seed)
      (ignore-errors (delete-file seed))
      t)))

(defun cursor-acp--review--normalize-rel (rel)
  (replace-regexp-in-string "\\\\" "/" (or rel "")))

(defun cursor-acp--review-skip-path-p (sess abs)
  (when (and (cursor-acp--valid-session-p sess) (stringp abs))
    (let* ((root (file-name-as-directory (expand-file-name (cursor-acp--workspace-root sess))))
           (rel (cursor-acp--review--normalize-rel
                 (file-relative-name (expand-file-name abs) root))))
      (unless (or (string-empty-p rel) (string-prefix-p ".." rel))
        (cl-some
         (lambda (pat)
           (let ((pat (string-trim (cursor-acp--review--normalize-rel (or pat "")))))
             (unless (string-empty-p pat)
               (let* ((pat (replace-regexp-in-string "\\`\\./+" "" pat))
                      (base (string-trim-right pat "/")))
                 (or (string-equal rel pat)
                     (string-equal rel base)
                     (string-prefix-p (concat base "/") rel))))))
         cursor-acp-review-skip-relative-prefixes)))))

(defun cursor-acp--review-clear-project-seeds (sess)
  (let ((proj (cursor-acp--review-project-dir sess)))
    (when (file-directory-p proj)
      (delete-directory proj t))))

(defun cursor-acp--review-seed-text (sess abs-path)
  (let ((seed (cursor-acp--review-seed-path sess abs-path)))
    (when (file-readable-p seed)
      (with-temp-buffer
        (insert-file-contents seed)
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun cursor-acp--review-file-text (abs-path)
  (cond
   ((not (stringp abs-path)) "")
   ((buffer-live-p (get-file-buffer abs-path))
    (with-current-buffer (get-file-buffer abs-path)
      (save-excursion
        (save-restriction
          (widen)
          (buffer-substring-no-properties (point-min) (point-max))))))
   ((file-readable-p abs-path)
    (with-temp-buffer
      (insert-file-contents abs-path)
      (buffer-substring-no-properties (point-min) (point-max))))
   (t "")))

(defun cursor-acp--ensure-sessions-table ()
  (unless (hash-table-p cursor-acp--sessions)
    (setq cursor-acp--sessions (make-hash-table :test #'equal)))
  cursor-acp--sessions)

(defun cursor-acp--ensure-conn ()
  (let ((c cursor-acp--conn))
    (unless (and c (ignore-errors (cursor-acp--conn-next-id c)))
      (setq c (cursor-acp--make-conn
               :process nil
               :stdout-acc ""
               :pending (make-hash-table :test #'eql)
               :next-id 1
               :init-result nil))
      (setq cursor-acp--conn c))
    c))

(defun cursor-acp--session-buffer-title (sess)
  (let ((t0 (string-trim (or (cursor-acp--session-title sess) ""))))
    (if (string-empty-p t0) "Untitled" t0)))

(defun cursor-acp--chat-buffer-name (sess)
  (format "*cursor-acp: %s*" (cursor-acp--session-buffer-title sess)))

(defun cursor-acp--input-buffer-name (sess)
  (format "*cursor-acp-input: %s*" (cursor-acp--session-buffer-title sess)))

(defun cursor-acp--rename-session-buffers (sess)
  (when (cursor-acp--valid-session-p sess)
    (when-let ((chat (cursor-acp--session-chat-buffer sess)))
      (when (buffer-live-p chat)
        (with-current-buffer chat
          (rename-buffer (generate-new-buffer-name (cursor-acp--chat-buffer-name sess)) t))))
    (when-let ((input (cursor-acp--session-input-buffer sess)))
      (when (buffer-live-p input)
        (with-current-buffer input
          (rename-buffer (generate-new-buffer-name (cursor-acp--input-buffer-name sess)) t))))))

(defun cursor-acp--session-by-id (session-id)
  (when (and (stringp session-id) (not (string-empty-p session-id)))
    (gethash session-id (cursor-acp--ensure-sessions-table) nil)))

(defun cursor-acp--ensure-session-by-id (session-id &optional title cwd)
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (error "Missing sessionId"))
  (let* ((tab (cursor-acp--ensure-sessions-table))
         (sess (gethash session-id tab nil)))
    (unless (cursor-acp--valid-session-p sess)
      (setq sess nil))
    (unless sess
      (setq sess (cursor-acp--make-session
                  :session-id session-id
                  :title (or title "Untitled")
                  :cwd (and (stringp cwd) (not (string-empty-p (string-trim cwd))) (expand-file-name cwd))
                  :current-mode "agent"
                  :current-model nil
                  :config-options nil
                  :available-modes nil
                  :available-models nil
                  :available-commands nil
                  :ui-expanded (make-hash-table :test #'equal)
                  :tool-calls (make-hash-table :test #'equal)
                  :plan-entries nil
                  :plan-buffer nil
                  :awaiting-create-plan nil
                  :create-plan-request-id nil
                  :create-plan-request-params nil
                  :create-plan-path nil
                  :busy nil
                  :spinner-idx 0
                  :spinner-timer nil
                  :awaiting-permission nil
                  :permission-request-id nil
                  :permission-request-ids nil
                  :permission-tool-call-id nil
                  :permission-request-params nil
                  :permission-response-cache (make-hash-table :test #'equal)
                  :awaiting-ask-question nil
                  :ask-question-request-id nil
                  :ask-question-request-ids nil
                  :ask-question-tool-call-id nil
                  :ask-question-request-params nil
                  :ask-question-response-cache (make-hash-table :test #'equal)
                  :assistant-frag ""
                  :draft-input ""
                  :main-buffer nil
                  :chat-buffer nil
                  :input-buffer nil
                  :info-buffer nil
                  :log-buffer nil))
      (cursor-acp--ui-set-expanded sess "Keys" t)
      (cursor-acp--ui-set-expanded sess "Modes" t)
      (cursor-acp--ui-set-expanded sess "Models" t)
      (cursor-acp--ui-set-expanded sess "Commands" t)
      (puthash session-id sess tab))
    (when (and (stringp title) (not (string-empty-p (string-trim title))))
      (setf (cursor-acp--session-title sess) title))
    (when (and (stringp cwd) (not (string-empty-p (string-trim cwd))))
      (setf (cursor-acp--session-cwd sess) (expand-file-name cwd)))
    (cursor-acp--ensure-session-buffers sess)
    (cursor-acp--rename-session-buffers sess)
    sess))

(defun cursor-acp--set-active-session-id (session-id)
  (setq cursor-acp--active-session-id session-id))

(defun cursor-acp--rehome-active-session-after-removal (removed-session-id)
  (unless (and (stringp removed-session-id)
               (stringp cursor-acp--active-session-id)
               (string-equal cursor-acp--active-session-id removed-session-id))
    (cl-return-from cursor-acp--rehome-active-session-after-removal nil))
  (let ((survivor
         (catch 'found
           (when (hash-table-p cursor-acp--sessions)
             (maphash
              (lambda (_k sess)
                (when (and (cursor-acp--valid-session-p sess)
                           (let ((sid (cursor-acp--session-session-id sess)))
                             (and (stringp sid)
                                  (not (string-empty-p sid))
                                  (not (string-equal sid removed-session-id)))))
                  (throw 'found (cursor-acp--session-session-id sess))))
              cursor-acp--sessions))
           nil)))
    (if (stringp survivor)
        (setq cursor-acp--active-session-id survivor)
      (let ((boot (gethash "__bootstrap__" (cursor-acp--ensure-sessions-table) nil)))
        (if (cursor-acp--valid-session-p boot)
            (setq cursor-acp--active-session-id "__bootstrap__")
          (setq cursor-acp--active-session-id nil))))))

(defun cursor-acp--session-local-evict (sess)
  (unless (and (cursor-acp--valid-session-p sess)
               (let ((sid (cursor-acp--session-session-id sess)))
                 (and (stringp sid) (not (string-empty-p sid)))))
    (cl-return-from cursor-acp--session-local-evict nil))
  (cursor-acp--session-transcript-save sess)
  (cursor-acp--set-busy sess nil)
  (when-let ((tmr (ignore-errors (cursor-acp--session-spinner-timer sess))))
    (ignore-errors (cancel-timer tmr)))
  (setf (cursor-acp--session-spinner-timer sess) nil)
  (setf (cursor-acp--session-awaiting-create-plan sess) nil)
  (setf (cursor-acp--session-create-plan-request-id sess) nil)
  (setf (cursor-acp--session-create-plan-request-params sess) nil)
  (setf (cursor-acp--session-create-plan-path sess) nil)
  (let ((sid (cursor-acp--session-session-id sess))
        (tab (cursor-acp--ensure-sessions-table)))
    (dolist (buf (list (ignore-errors (cursor-acp--session-chat-buffer sess))
                       (ignore-errors (cursor-acp--session-input-buffer sess))
                       (ignore-errors (cursor-acp--session-plan-buffer sess))))
      (when (buffer-live-p buf)
        (ignore-errors (kill-buffer buf))))
    (remhash sid tab)))

(defun cursor-acp--active-session ()
  (cursor-acp--session-by-id cursor-acp--active-session-id))

(defun cursor-acp--session-for-buffer (buf)
  "Return the session that owns BUF (chat/input/plan/info/log), else nil."
  (when (and (buffer-live-p buf) (hash-table-p cursor-acp--sessions))
    (catch 'found
      (maphash
       (lambda (_k sess)
         (when (and (cursor-acp--valid-session-p sess)
                    (memq buf (list (cursor-acp--session-chat-buffer sess)
                                    (cursor-acp--session-input-buffer sess)
                                    (cursor-acp--session-plan-buffer sess)
                                    (cursor-acp--session-info-buffer sess)
                                    (cursor-acp--session-log-buffer sess))))
           (throw 'found sess)))
       cursor-acp--sessions)
      nil)))

(defun cursor-acp--ensure-buffer (name)
  (get-buffer-create name))

(defun cursor-acp--ensure-unique-buffer (name)
  (get-buffer-create (generate-new-buffer-name name)))

(defun cursor-acp--count-words (s)
  (length (split-string (or s "") "[ \t\n\r]+" t)))

(defun cursor-acp-reset ()
  "Reset all in-memory session state and kill the ACP process if running."
  (interactive)
  (when-let ((conn cursor-acp--conn))
    (when-let ((proc (ignore-errors (cursor-acp--conn-process conn))))
      (when (process-live-p proc)
        (ignore-errors (delete-process proc)))))
  (when (hash-table-p cursor-acp--sessions)
    (maphash
     (lambda (_sid sess)
       (when-let ((tmr (ignore-errors (cursor-acp--session-spinner-timer sess))))
         (ignore-errors (cancel-timer tmr)))
       (dolist (buf (list (ignore-errors (cursor-acp--session-chat-buffer sess))
                          (ignore-errors (cursor-acp--session-input-buffer sess))
                          (ignore-errors (cursor-acp--session-plan-buffer sess))
                          (ignore-errors (cursor-acp--session-info-buffer sess))
                          (ignore-errors (cursor-acp--session-log-buffer sess))))
         (when (buffer-live-p buf)
           (ignore-errors (kill-buffer buf)))))
     cursor-acp--sessions))
  (setq cursor-acp--conn nil)
  (setq cursor-acp--sessions nil)
  (setq cursor-acp--active-session-id nil)
  (message "cursor-acp session reset"))

(defun cursor-acp--valid-session-p (sess)
  (and sess
       (condition-case nil
           (progn
             (cursor-acp--session-current-mode sess)
             (cursor-acp--session-available-commands sess)
             (cursor-acp--session-ui-expanded sess)
             (cursor-acp--session-chat-buffer sess)
             (cursor-acp--session-log-buffer sess)
             t)
         (error nil))))

(defun cursor-acp--ui-set-expanded (sess section expanded)
  (let ((h (cursor-acp--session-ui-expanded sess)))
    (when (hash-table-p h)
      (puthash section expanded h))))

(defun cursor-acp--ensure-session-buffers (sess)
  (let ((chat (cursor-acp--session-chat-buffer sess))
        (input (cursor-acp--session-input-buffer sess))
        (plan (cursor-acp--session-plan-buffer sess))
        (info (cursor-acp--session-info-buffer sess))
        (log (cursor-acp--session-log-buffer sess)))
    (unless (buffer-live-p chat)
      (setf (cursor-acp--session-chat-buffer sess)
            (cursor-acp--ensure-unique-buffer (cursor-acp--chat-buffer-name sess))))
    (unless (buffer-live-p input)
      (setf (cursor-acp--session-input-buffer sess)
            (cursor-acp--ensure-unique-buffer (cursor-acp--input-buffer-name sess))))
    (when (and plan (not (buffer-live-p plan)))
      (setf (cursor-acp--session-plan-buffer sess) nil))
    (unless (buffer-live-p info)
      (setf (cursor-acp--session-info-buffer sess)
            (cursor-acp--ensure-buffer cursor-acp--info-buffer-name)))
    (unless (buffer-live-p log)
      (setf (cursor-acp--session-log-buffer sess)
            (cursor-acp--ensure-buffer cursor-acp--log-buffer-name)))
    (with-current-buffer (cursor-acp--session-log-buffer sess)
      (setq-local truncate-lines nil)
      (setq-local word-wrap t)))
  sess)

(defun cursor-acp--ensure-session ()
  "Return the active session, creating a bootstrap session if needed."
  (cursor-acp--ensure-conn)
  (cursor-acp--ensure-sessions-table)
  (let* ((sid cursor-acp--active-session-id)
         (sess (and (stringp sid) (cursor-acp--session-by-id sid))))
    (unless (cursor-acp--valid-session-p sess)
      (setq sess nil))
    (unless sess
      (setq sid "__bootstrap__")
      (setq sess (cursor-acp--make-session
                  :session-id nil
                  :title "New Session"
                  :cwd nil
                  :current-mode "agent"
                  :current-model nil
                  :config-options nil
                  :available-modes nil
                  :available-models nil
                  :available-commands nil
                  :ui-expanded (make-hash-table :test #'equal)
                  :tool-calls (make-hash-table :test #'equal)
                  :plan-entries nil
                  :plan-buffer nil
                  :awaiting-create-plan nil
                  :create-plan-request-id nil
                  :create-plan-request-params nil
                  :create-plan-path nil
                  :busy nil
                  :spinner-idx 0
                  :spinner-timer nil
                  :awaiting-permission nil
                  :permission-request-id nil
                  :permission-request-ids nil
                  :permission-tool-call-id nil
                  :permission-request-params nil
                  :permission-response-cache (make-hash-table :test #'equal)
                  :awaiting-ask-question nil
                  :ask-question-request-id nil
                  :ask-question-request-ids nil
                  :ask-question-tool-call-id nil
                  :ask-question-request-params nil
                  :ask-question-response-cache (make-hash-table :test #'equal)
                  :assistant-frag ""
                  :draft-input ""
                  :main-buffer nil
                  :chat-buffer nil
                  :input-buffer nil
                  :info-buffer nil
                  :log-buffer nil))
      (cursor-acp--ui-set-expanded sess "Keys" t)
      (cursor-acp--ui-set-expanded sess "Modes" t)
      (cursor-acp--ui-set-expanded sess "Models" t)
      (cursor-acp--ui-set-expanded sess "Commands" t)
      (puthash sid sess cursor-acp--sessions)
      (setq cursor-acp--active-session-id sid))
    (cursor-acp--ensure-session-buffers sess)
    sess))

(defun cursor-acp--workspace-root (sess)
  "Return workspace root directory for SESS (the session's stored cwd)."
  (let ((cwd (and (cursor-acp--valid-session-p sess)
                  (cursor-acp--session-cwd sess))))
    (file-name-as-directory (expand-file-name (or cwd default-directory)))))

(defun cursor-acp--plans-dir (sess)
  (expand-file-name ".cursor/plans/" (cursor-acp--workspace-root sess)))

(defun cursor-acp--sanitize-plan-name (name)
  (let* ((s (string-trim (or name "")))
         (s (if (string-empty-p s) "plan" s))
         (s (downcase s))
         (s (replace-regexp-in-string "[^a-z0-9._-]+" "_" s))
         (s (replace-regexp-in-string "_+" "_" s))
         (s (replace-regexp-in-string "\\`[_\\. -]+" "" s))
         (s (replace-regexp-in-string "[_\\. -]+\\'" "" s)))
    (if (string-empty-p s) "plan" s)))

(defun cursor-acp--plan-path (sess name)
  (expand-file-name (format "%s.md" (cursor-acp--sanitize-plan-name name))
                    (cursor-acp--plans-dir sess)))

(defun cursor-acp--json-encode (obj)
  (json-serialize obj :null-object :null :false-object :json-false))

(defun cursor-acp--log (sess direction payload)
  (let ((buf (cursor-acp--session-log-buffer sess)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
        (insert direction "\n")
        (condition-case _
            (cond
             ((stringp payload)
              (let* ((obj (ignore-errors (json-parse-string payload :object-type 'hash-table :array-type 'list))))
                (insert (if obj (json-serialize obj :pretty t) payload))))
             (t
              (insert (json-serialize payload :pretty t :null-object :null :false-object :json-false))))
          (error
           (insert (if (stringp payload) payload (format "%S" payload)))))
        (insert "\n\n")))))

(defun cursor-acp--chat-header-line (sess)
  (let* ((conn (cursor-acp--ensure-conn))
         (proc (cursor-acp--conn-process conn))
         (alive (and proc (process-live-p proc)))
         (busy (and alive (cursor-acp--session-busy sess)))
         (sp (when busy
               (let* ((idx (or (cursor-acp--session-spinner-idx sess) 0))
                      (n (length cursor-acp--spinner-frames)))
                 (aref cursor-acp--spinner-frames (mod idx n)))))
         (title (cursor-acp--session-buffer-title sess))
         (mode (or (cursor-acp--session-current-mode sess) "-"))
         (model0 (cursor-acp--session-current-model sess))
         (model1 (if (stringp model0) (string-trim model0) ""))
         (model (if (or (not (stringp model0))
                        (string-empty-p model1)
                        (string-equal model1 "-"))
                    "Auto"
                  model1)))
    (concat
     (if alive "🟢" "🔴")
     (when sp (format " %s" sp))
     "  "
     (propertize (format "%s  %s  %s" title mode model) 'face 'cursor-acp-header-face))))

(defun cursor-acp--chat-refresh-header (sess)
  (when (cursor-acp--valid-session-p sess)
    (when-let ((buf (cursor-acp--session-chat-buffer sess)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq-local header-line-format (cursor-acp--chat-header-line sess)))))))

(defun cursor-acp--status-line (sess)
  (let* ((conn (cursor-acp--ensure-conn))
         (proc (cursor-acp--conn-process conn))
         (alive (and proc (process-live-p proc)))
         (sid (cursor-acp--session-session-id sess))
         (mode (or (cursor-acp--session-current-mode sess) "-"))
         (model (or (cursor-acp--session-current-model sess) "-"))
         (busy (and alive (cursor-acp--session-busy sess)))
         (awaiting (and alive (cursor-acp--session-awaiting-permission sess)))
         (awaiting-q (and alive (cursor-acp--session-awaiting-ask-question sess)))
         (sp (when busy
               (let* ((idx (or (cursor-acp--session-spinner-idx sess) 0))
                      (n (length cursor-acp--spinner-frames)))
                 (aref cursor-acp--spinner-frames (mod idx n))))))
    (concat
     (propertize "Cursor ACP" 'face 'cursor-acp-header-face)
     "  "
     (propertize (if alive "connected" "disconnected")
                 'face (if alive 'cursor-acp-status-ok-face 'cursor-acp-status-bad-face))
     (when sp (format " %s" sp))
     (when awaiting " awaiting-permission")
     (when awaiting-q " awaiting-ask-question")
     (when sid (format "  sessionId=%s" sid))
     (format "  title=%s" (cursor-acp--session-buffer-title sess))
     (format "  mode=%s  model=%s" mode model))))

(defun cursor-acp--refresh-header (sess)
  (cursor-acp--chat-refresh-header sess))

(defun cursor-acp--spinner-start (sess)
  (when-let ((tmr (cursor-acp--session-spinner-timer sess)))
    (ignore-errors (cancel-timer tmr)))
  (setf (cursor-acp--session-spinner-idx sess) 0)
  (setf (cursor-acp--session-spinner-timer sess)
        (run-at-time
         0 cursor-acp--spinner-interval
         (lambda ()
           (when-let ((s (cursor-acp--active-session)))
             (when (cursor-acp--session-busy s)
               (setf (cursor-acp--session-spinner-idx s)
                     (1+ (or (cursor-acp--session-spinner-idx s) 0)))
               (cursor-acp--refresh-header s)))))))

(defun cursor-acp--spinner-stop (sess)
  (when-let ((tmr (cursor-acp--session-spinner-timer sess)))
    (ignore-errors (cancel-timer tmr)))
  (setf (cursor-acp--session-spinner-timer sess) nil)
  (setf (cursor-acp--session-spinner-idx sess) 0))

(defun cursor-acp--set-busy (sess busy)
  (setf (cursor-acp--session-busy sess) (and busy t))
  (if busy
      (cursor-acp--spinner-start sess)
    (cursor-acp--spinner-stop sess))
  (cursor-acp--refresh-header sess))

(defun cursor-acp--set-awaiting-permission (sess awaiting)
  (setf (cursor-acp--session-awaiting-permission sess) (and awaiting t))
  (cursor-acp--refresh-header sess))

(defun cursor-acp--set-awaiting-ask-question (sess awaiting)
  (setf (cursor-acp--session-awaiting-ask-question sess) (and awaiting t))
  (cursor-acp--refresh-header sess))

(defun cursor-acp--ht-get (ht key)
  (when (hash-table-p ht) (gethash key ht nil)))

(defun cursor-acp--deep-get (ht keys)
  (let ((cur ht))
    (catch 'missing
      (dolist (k keys)
        (setq cur (cursor-acp--ht-get cur k))
        (unless cur (throw 'missing nil)))
      cur)))

(defun cursor-acp--normalize-items (items)
  (cond
   ((vectorp items) (append items nil))
   ((listp items) items)
   (t nil)))

(defun cursor-acp--plan-todo-id-string (entry)
  (when (hash-table-p entry)
    (let ((id (cursor-acp--ht-get entry "id")))
      (when (stringp id)
        (let ((s (string-trim id)))
          (unless (string-empty-p s) s))))))

(defun cursor-acp--merge-plan-todo-entries (existing incoming mergep)
  (let ((inc (cursor-acp--normalize-items incoming)))
    (if (not mergep)
        inc
      (let* ((ex (or (cursor-acp--normalize-items existing) '()))
             (by-id (make-hash-table :test 'equal))
             out)
        (dolist (item inc)
          (when-let ((id (cursor-acp--plan-todo-id-string item)))
            (puthash id item by-id)))
        (dolist (e ex)
          (let* ((id (cursor-acp--plan-todo-id-string e))
                 (rep (and id (gethash id by-id))))
            (if rep
                (progn (remhash id by-id) (push rep out))
              (push e out))))
        (setq out (nreverse out))
        (dolist (item inc)
          (when-let ((id (cursor-acp--plan-todo-id-string item)))
            (when (gethash id by-id)
              (setq out (append out (list (gethash id by-id))))
              (remhash id by-id))))
        (dolist (item inc)
          (unless (cursor-acp--plan-todo-id-string item)
            (setq out (append out (list item)))))
        out))))

(defun cursor-acp--plan-update-todos-merge-p (params)
  (unless (hash-table-p params)
    (cl-return-from cursor-acp--plan-update-todos-merge-p t))
  (let ((m (cursor-acp--ht-get params "merge")))
    (cond
     ((null m) t)
     ((eq m json-false) nil)
     ((stringp m)
      (not (member (downcase (string-trim m)) '("false" "0" "no"))))
     (t (and m t)))))

(defun cursor-acp--cache-config-options (sess opts)
  (when-let ((os (cursor-acp--normalize-items opts)))
    (setf (cursor-acp--session-config-options sess) os)))

(defun cursor-acp--sync-current-from-config-options (sess)
  (let* ((opts (or (cursor-acp--session-config-options sess) '()))
         (mode-opt
          (or (cl-find-if (lambda (o) (and (hash-table-p o)
                                           (string-equal (cursor-acp--ht-get o "category") "mode")))
                          opts)
              (cl-find-if (lambda (o) (and (hash-table-p o)
                                           (string-equal (cursor-acp--ht-get o "id") "mode")))
                          opts)))
         (model-opt
          (or (cl-find-if (lambda (o) (and (hash-table-p o)
                                           (string-equal (cursor-acp--ht-get o "category") "model")))
                          opts)
              (cl-find-if (lambda (o) (and (hash-table-p o)
                                           (string-equal (cursor-acp--ht-get o "id") "model")))
                          opts)))
         (mode (and (hash-table-p mode-opt) (cursor-acp--ht-get mode-opt "currentValue")))
         (model (and (hash-table-p model-opt) (cursor-acp--ht-get model-opt "currentValue"))))
    (when (stringp mode) (setf (cursor-acp--session-current-mode sess) mode))
    (when (stringp model) (setf (cursor-acp--session-current-model sess) model))))

(defun cursor-acp--default-modes ()
  (list (let ((ht (make-hash-table :test 'equal)))
          (puthash "id" "agent" ht)
          (puthash "description" "Full tool access." ht)
          ht)
        (let ((ht (make-hash-table :test 'equal)))
          (puthash "id" "plan" ht)
          (puthash "description" "Planning mode (read-only behavior)." ht)
          ht)
        (let ((ht (make-hash-table :test 'equal)))
          (puthash "id" "ask" ht)
          (puthash "description" "Q&A / read-only behavior." ht)
          ht)))

(defun cursor-acp--cache-capabilities (sess init-result)
  (let* ((agent-cap (cursor-acp--ht-get init-result "agentCapabilities"))
         (modes (or (cursor-acp--ht-get init-result "modes")
                    (cursor-acp--ht-get agent-cap "modes")))
         (models (or (cursor-acp--ht-get init-result "models")
                     (cursor-acp--ht-get agent-cap "models")))
         (commands (or (cursor-acp--ht-get init-result "commands")
                       (cursor-acp--ht-get agent-cap "commands")
                       (cursor-acp--ht-get agent-cap "availableCommands"))))
    (setf (cursor-acp--session-available-modes sess)
          (or (cursor-acp--normalize-items modes)
              (cursor-acp--default-modes)))
    (setf (cursor-acp--session-available-models sess) (cursor-acp--normalize-items models))
    (setf (cursor-acp--session-available-commands sess) (cursor-acp--normalize-items commands))))

(defun cursor-acp--cache-session-new (sess res)
  (let* ((modes-obj (cursor-acp--ht-get res "modes"))
         (models-obj (cursor-acp--ht-get res "models"))
         (config-obj (cursor-acp--ht-get res "configOptions"))
         (mode (or (cursor-acp--ht-get modes-obj "currentModeId")
                   (cursor-acp--ht-get res "mode")
                   (cursor-acp--ht-get res "currentMode")))
         (model (or (cursor-acp--ht-get models-obj "currentModelId")
                    (cursor-acp--ht-get res "model")
                    (cursor-acp--ht-get res "currentModel")))
         (modes (or (cursor-acp--ht-get modes-obj "availableModes")
                    (cursor-acp--ht-get res "availableModes")
                    (cursor-acp--ht-get res "modes")))
         (models (or (cursor-acp--ht-get models-obj "availableModels")
                     (cursor-acp--ht-get res "availableModels")
                     (cursor-acp--ht-get res "models")))
         (commands (or (cursor-acp--ht-get res "commands")
                       (cursor-acp--ht-get res "availableCommands"))))
    (cursor-acp--cache-config-options sess config-obj)
    (when (stringp mode) (setf (cursor-acp--session-current-mode sess) mode))
    (when (stringp model) (setf (cursor-acp--session-current-model sess) model))
    (cursor-acp--sync-current-from-config-options sess)
    (when-let ((ms (cursor-acp--normalize-items modes)))
      (setf (cursor-acp--session-available-modes sess) ms))
    (when-let ((ms (cursor-acp--normalize-items models)))
      (setf (cursor-acp--session-available-models sess) ms))
    (when-let ((cs (cursor-acp--normalize-items commands)))
      (setf (cursor-acp--session-available-commands sess) cs))
    (cursor-acp--log
     sess "cache"
     (cursor-acp--json-encode
      `((source . "session/new")
        (currentMode . ,(cursor-acp--session-current-mode sess))
        (currentModel . ,(cursor-acp--session-current-model sess))
        (configOptionsCount . ,(length (or (cursor-acp--session-config-options sess) '())))
        (modesCount . ,(length (or (cursor-acp--session-available-modes sess) '())))
        (modelsCount . ,(length (or (cursor-acp--session-available-models sess) '())))
        (commandsCount . ,(length (or (cursor-acp--session-available-commands sess) '()))))))))

(defun cursor-acp--maybe-cache-commands-from-update (sess update)
  (let* ((cmds (or (cursor-acp--ht-get update "commands")
                   (cursor-acp--ht-get update "availableCommands")
                   (cursor-acp--ht-get update "toolCommands")
                   (cursor-acp--deep-get update '("capabilities" "commands")))))
    (when-let ((cs (cursor-acp--normalize-items cmds)))
      (setf (cursor-acp--session-available-commands sess) cs)
      (cursor-acp--log
       sess "cache"
       (cursor-acp--json-encode
        `((source . "session/update")
          (commandsCount . ,(length cs))))))))

(defun cursor-acp--maybe-cache-config-options-from-update (sess update)
  (let* ((opts (or (cursor-acp--ht-get update "configOptions")
                   (cursor-acp--deep-get update '("configOptions")))))
    (when-let ((os (cursor-acp--normalize-items opts)))
      (setf (cursor-acp--session-config-options sess) os)
      (cursor-acp--sync-current-from-config-options sess)
      (cursor-acp--log
       sess "cache"
       (cursor-acp--json-encode
        `((source . "session/update")
          (configOptionsCount . ,(length os))))))))

(defun cursor-acp--ui-expanded-p (sess section)
  (let ((h (cursor-acp--session-ui-expanded sess)))
    (if (hash-table-p h) (gethash section h nil) nil)))

(defun cursor-acp--config-option-by-category (sess category)
  (cl-find-if
   (lambda (it)
     (and (hash-table-p it)
          (stringp (cursor-acp--ht-get it "category"))
          (string-equal (cursor-acp--ht-get it "category") category)))
   (or (cursor-acp--session-config-options sess) '())))

(defun cursor-acp--config-option-by-id (sess config-id)
  (cl-find-if
   (lambda (it)
     (and (hash-table-p it)
          (stringp (cursor-acp--ht-get it "id"))
          (string-equal (cursor-acp--ht-get it "id") config-id)))
   (or (cursor-acp--session-config-options sess) '())))

(defun cursor-acp--config-option-select (_sess opt)
  (let* ((id (cursor-acp--ht-get opt "id"))
         (name (or (cursor-acp--ht-get opt "name") id))
         (cur (cursor-acp--ht-get opt "currentValue"))
         (options (cursor-acp--normalize-items (cursor-acp--ht-get opt "options")))
         (alist
          (mapcar
           (lambda (v)
             (let* ((val (cursor-acp--ht-get v "value"))
                    (vname (or (cursor-acp--ht-get v "name") val))
                    (label (if (and cur (stringp cur) (stringp val) (string-equal cur val))
                               (format "%s (current)" vname)
                             (format "%s" vname))))
               (cons label val)))
           (cl-remove-if-not #'hash-table-p (or options '()))))
         (choice (completing-read (format "%s: " (or name "Select")) (mapcar #'car alist) nil t)))
    (cdr (assoc choice alist))))

(provide 'cursor-acp-core)
;;; cursor-acp-core.el ends here
