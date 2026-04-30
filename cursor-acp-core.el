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
  busy
  spinner-idx
  spinner-timer
  awaiting-permission
  permission-request-id
  permission-request-params
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
  "If non-nil, hide Markdown markup characters in chat buffer.

This uses `markdown-mode' markup-hiding overlays when available."
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

(defun cursor-acp--review--sanitize (s)
  (replace-regexp-in-string "[^A-Za-z0-9_.-]" "_" (or s "")))

(defun cursor-acp--review-project-dir (sess)
  (let* ((root (cursor-acp--workspace-root sess))
         (key (cursor-acp--review--sanitize (directory-file-name (expand-file-name root)))))
    (file-name-as-directory (expand-file-name key cursor-acp-review-root-dir))))

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
                  :busy nil
                  :spinner-idx 0
                  :spinner-timer nil
                  :awaiting-permission nil
                  :permission-request-id nil
                  :permission-request-params nil
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

(defun cursor-acp--active-session ()
  (cursor-acp--session-by-id cursor-acp--active-session-id))

(defun cursor-acp--session-for-buffer (buf)
  "Return the session that owns BUF (chat/input/info/log), else nil."
  (when (and (buffer-live-p buf) (hash-table-p cursor-acp--sessions))
    (catch 'found
      (maphash
       (lambda (_k sess)
         (when (and (cursor-acp--valid-session-p sess)
                    (memq buf (list (cursor-acp--session-chat-buffer sess)
                                    (cursor-acp--session-input-buffer sess)
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
        (info (cursor-acp--session-info-buffer sess))
        (log (cursor-acp--session-log-buffer sess)))
    (unless (buffer-live-p chat)
      (setf (cursor-acp--session-chat-buffer sess)
            (cursor-acp--ensure-unique-buffer (cursor-acp--chat-buffer-name sess))))
    (unless (buffer-live-p input)
      (setf (cursor-acp--session-input-buffer sess)
            (cursor-acp--ensure-unique-buffer (cursor-acp--input-buffer-name sess))))
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
                  :busy nil
                  :spinner-idx 0
                  :spinner-timer nil
                  :awaiting-permission nil
                  :permission-request-id nil
                  :permission-request-params nil
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

(defun cursor-acp--status-line (sess)
  (let* ((conn (cursor-acp--ensure-conn))
         (proc (cursor-acp--conn-process conn))
         (alive (and proc (process-live-p proc)))
         (sid (cursor-acp--session-session-id sess))
         (mode (or (cursor-acp--session-current-mode sess) "-"))
         (model (or (cursor-acp--session-current-model sess) "-"))
         (busy (and alive (cursor-acp--session-busy sess)))
         (awaiting (and alive (cursor-acp--session-awaiting-permission sess)))
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
     (when sid (format "  sessionId=%s" sid))
     (format "  title=%s" (cursor-acp--session-buffer-title sess))
     (format "  mode=%s  model=%s" mode model))))

(defun cursor-acp--refresh-header (sess)
  (when (cursor-acp--valid-session-p sess)
    (when-let ((buf (cursor-acp--session-chat-buffer sess)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq-local header-line-format (cursor-acp--status-line sess)))))))

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
