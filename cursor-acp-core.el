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

(cl-defstruct (cursor-acp--session (:constructor cursor-acp--make-session))
  process
  stdout-acc
  pending
  next-id
  session-id
  current-mode
  current-model
  config-options
  available-modes
  available-models
  available-commands
  ui-expanded
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

(defvar cursor-acp--current-session nil)

(defconst cursor-acp--chat-buffer-name "*cursor-acp*")
(defconst cursor-acp--input-buffer-name "*cursor-acp-input*")
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

(defun cursor-acp--ensure-buffer (name)
  (get-buffer-create name))

(defun cursor-acp--count-words (s)
  (length (split-string (or s "") "[ \t\n\r]+" t)))

(defun cursor-acp-reset ()
  "Reset the in-memory session state and kill the ACP process if running."
  (interactive)
  (when-let ((sess cursor-acp--current-session))
    (when-let ((tmr (ignore-errors (cursor-acp--session-spinner-timer sess))))
      (ignore-errors (cancel-timer tmr)))
    (when-let ((proc (ignore-errors (cursor-acp--session-process sess))))
      (when (process-live-p proc)
        (ignore-errors (delete-process proc))))
    (dolist (buf (list (ignore-errors (cursor-acp--session-chat-buffer sess))
                       (ignore-errors (cursor-acp--session-input-buffer sess))
                       (ignore-errors (cursor-acp--session-info-buffer sess))
                       (ignore-errors (cursor-acp--session-log-buffer sess))))
      (when (buffer-live-p buf)
        (ignore-errors (kill-buffer buf)))))
  (setq cursor-acp--current-session nil)
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

(defun cursor-acp--ensure-session ()
  (let ((sess cursor-acp--current-session))
    (unless (cursor-acp--valid-session-p sess)
      (setq sess nil)
      (setq cursor-acp--current-session nil))
    (unless sess
      (setq sess (cursor-acp--make-session
                  :process nil
                  :stdout-acc ""
                  :pending (make-hash-table :test #'eql)
                  :next-id 1
                  :session-id nil
                  :current-mode "agent"
                  :current-model nil
                  :config-options nil
                  :available-modes nil
                  :available-models nil
                  :available-commands nil
                  :ui-expanded (make-hash-table :test #'equal)
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
      (setq cursor-acp--current-session sess))
    (let ((chat (cursor-acp--session-chat-buffer sess))
          (input (cursor-acp--session-input-buffer sess))
          (info (cursor-acp--session-info-buffer sess))
          (log (cursor-acp--session-log-buffer sess)))
      (unless (buffer-live-p chat)
        (setf (cursor-acp--session-chat-buffer sess)
              (cursor-acp--ensure-buffer cursor-acp--chat-buffer-name)))
      (unless (buffer-live-p input)
        (setf (cursor-acp--session-input-buffer sess)
              (cursor-acp--ensure-buffer cursor-acp--input-buffer-name)))
      (unless (buffer-live-p info)
        (setf (cursor-acp--session-info-buffer sess)
              (cursor-acp--ensure-buffer cursor-acp--info-buffer-name)))
      (unless (buffer-live-p log)
        (setf (cursor-acp--session-log-buffer sess)
              (cursor-acp--ensure-buffer cursor-acp--log-buffer-name)))
      (with-current-buffer (cursor-acp--session-log-buffer sess)
        (setq-local truncate-lines nil)
        (setq-local word-wrap t)))
    sess))

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
  (let* ((proc (cursor-acp--session-process sess))
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
        (run-at-time 0 cursor-acp--spinner-interval
                     (lambda ()
                       (when (cursor-acp--valid-session-p cursor-acp--current-session)
                         (let ((s cursor-acp--current-session))
                           (when (cursor-acp--session-busy s)
                             (setf (cursor-acp--session-spinner-idx s)
                                   (1+ (or (cursor-acp--session-spinner-idx s) 0)))
                             (cursor-acp--refresh-header s))))))))

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
