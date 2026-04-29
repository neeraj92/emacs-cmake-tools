;;; cursor-acp.el --- Cursor ACP client -*- lexical-binding: t; -*-

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

;; (Temp file + write-through was only needed for `markdown-live-preview-mode`.)

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
        (ignore-errors (delete-process proc)))))
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
        (setq-local truncate-lines t)))
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

(defun cursor-acp--ui-set-expanded (sess section expanded)
  (let ((h (cursor-acp--session-ui-expanded sess)))
    (when (hash-table-p h)
      (puthash section expanded h))))

(defvar-local cursor-acp--assistant-open nil)
(defvar-local cursor-acp--assistant-start nil)

(defun cursor-acp--insert-section (sess name items &optional empty-note)
  (let* ((expanded (cursor-acp--ui-expanded-p sess name))
         (marker (if expanded "▼" "▶")))
    (insert (format "%s %s\n" marker name))
    (when expanded
      (if items
          (cursor-acp--insert-items items)
        (insert "  " (or empty-note "(none)") "\n")))))

(defun cursor-acp--section-at-point ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^[▶▼] \\(Keys\\|Modes\\|Models\\|Commands\\|Transcript\\)\\s-*$")
      (match-string 1))))

(defun cursor-acp--insert-items (items)
  (if (not items)
      (insert "  (none)\n")
    (dolist (it items)
      (cond
       ((hash-table-p it)
        (let ((id (or (cursor-acp--ht-get it "id") (cursor-acp--ht-get it "name") (cursor-acp--ht-get it "command")))
              (name (or (cursor-acp--ht-get it "name") (cursor-acp--ht-get it "label")))
              (desc (or (cursor-acp--ht-get it "description") (cursor-acp--ht-get it "desc"))))
          (when (and (not id) (cursor-acp--ht-get it "modelId"))
            (setq id (cursor-acp--ht-get it "modelId")))
          (when (and (not id) (cursor-acp--ht-get it "value"))
            (setq id (cursor-acp--ht-get it "value")))
          (insert "  - ")
          (insert (format "%s" (or id name "-")))
          (when (and name id (not (equal name id)))
            (insert (format " (%s)" name)))
          (when desc
            (insert "\n    " (string-trim (format "%s" desc))))
          (insert "\n")))
       (t
        (insert "  - " (format "%s" it) "\n"))))))

(defun cursor-acp-toggle-section ()
  "Toggle the Modes/Models/Commands section at point."
  (interactive)
  (let* ((sess (cursor-acp--ensure-session))
         (section (or (cursor-acp--section-at-point)
                      (save-excursion
                        (while (and (not (bobp)) (not (cursor-acp--section-at-point)))
                          (forward-line -1))
                        (cursor-acp--section-at-point)))))
    (when section
      (cursor-acp--ui-set-expanded sess section (not (cursor-acp--ui-expanded-p sess section)))
      (cursor-acp--render-info sess))))

(defun cursor-acp--keys-items ()
  (list
   "C-c C-s  start/connect"
   "C-c C-k  hard stop"
   "C-c C-r  reset layout"
   "C-c C-l  show logs"
   "C-c C-i  show info"
   "C-c C-p  focus input"
   "RET      send (in input)"
   "C-j      newline (in input)"
   "TAB/C-i  toggle section"))

;; Input buffer helpers
(defun cursor-acp--input-text (sess)
  (with-current-buffer (cursor-acp--session-input-buffer sess)
    (string-trim-right (buffer-substring-no-properties (point-min) (point-max)))))

(defun cursor-acp--input-clear (sess)
  (with-current-buffer (cursor-acp--session-input-buffer sess)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun cursor-acp--ensure-input-window-height (sess)
  (let* ((buf (cursor-acp--session-input-buffer sess))
         (win (and (buffer-live-p buf) (get-buffer-window buf t))))
    (when (window-live-p win)
      (cond
       ((fboundp 'set-window-text-height)
        (ignore-errors (set-window-text-height win 5)))
       (t
        (let* ((cur (window-body-height win))
               (delta (- 5 cur)))
          (when (/= delta 0)
            (ignore-errors (window-resize win delta)))))))))

(defun cursor-acp--input-focus (sess)
  (let ((buf (cursor-acp--session-input-buffer sess)))
    (when (buffer-live-p buf)
      (pop-to-buffer buf)
      (cursor-acp--ensure-input-window-height sess)
      (goto-char (point-max))
      (when (and (featurep 'evil) (fboundp 'evil-insert-state))
        (evil-insert-state)))))

(defun cursor-acp-focus-input ()
  (interactive)
  (cursor-acp--input-focus (cursor-acp--ensure-session)))

(defun cursor-acp--acp-buffer-p (sess buf)
  (and (buffer-live-p buf)
       (memq buf
             (list (cursor-acp--session-chat-buffer sess)
                   (cursor-acp--session-input-buffer sess)
                   (cursor-acp--session-info-buffer sess)
                   (cursor-acp--session-log-buffer sess)))))

(defun cursor-acp--preferred-main-buffer ()
  (let* ((sess (cursor-acp--ensure-session))
         (chat (cursor-acp--session-chat-buffer sess))
         (input (cursor-acp--session-input-buffer sess))
         (info (cursor-acp--session-info-buffer sess))
         (log (cursor-acp--session-log-buffer sess))
         (cur (window-buffer (selected-window))))
    (if (memq cur (list chat input info log))
        (or (cl-find-if
             (lambda (b) (and (buffer-live-p b) (not (memq b (list chat input info log)))))
             (buffer-list))
            (other-buffer cur t))
      cur)))

(defun cursor-acp-reset-layout ()
  "Reset ACP windows in current frame to the default layout."
  (interactive)
  (let* ((sess (cursor-acp--ensure-session))
         (chat (cursor-acp--session-chat-buffer sess))
         (input (cursor-acp--session-input-buffer sess))
         (info (cursor-acp--session-info-buffer sess))
         (log (cursor-acp--session-log-buffer sess))
         (saved-main (cursor-acp--session-main-buffer sess))
         (main (if (cursor-acp--acp-buffer-p sess saved-main)
                   (cursor-acp--preferred-main-buffer)
                 saved-main))
         (wins (window-list (selected-frame) 'no-minibuf)))
    (dolist (w wins)
      (let ((b (window-buffer w)))
        (when (and (window-live-p w) (memq b (list info log)))
          (ignore-errors (delete-window w)))))
    (when (or (not (buffer-live-p main)) (minibufferp main))
      (setq main (cursor-acp--preferred-main-buffer)))
    (when (buffer-live-p main)
      (switch-to-buffer main))
    (delete-other-windows)
    (let* ((win (selected-window))
           (total (max 12 (window-total-height win)))
           (bottom-height (max 8 (min (- total 4) (floor (* total 0.35)))))
           (chat-win (split-window win (- bottom-height) 'below)))
      (set-window-buffer chat-win chat)
      (select-window chat-win)
      (let ((input-win (split-window chat-win 5 'below)))
        (set-window-buffer input-win input))
      (with-current-buffer chat
        (cursor-acp-chat-mode)
        (setq-local header-line-format (cursor-acp--status-line sess)))
      (with-current-buffer input
        (cursor-acp-input-mode))
      (cursor-acp--ensure-input-window-height sess)
      (cursor-acp--input-focus sess))))

(defun cursor-acp--chat-eob-p ()
  (>= (point) (max (point-min) (- (point-max) 2))))

(defun cursor-acp--chat-insert (sess s &optional read-only)
  (let* ((buf (cursor-acp--session-chat-buffer sess))
         (at-eob (with-current-buffer buf (cursor-acp--chat-eob-p))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (let ((start (point)))
          (insert s)
          (when read-only
            (put-text-property start (point) 'read-only t)))))
    (with-current-buffer buf
      (when (and (bound-and-true-p font-lock-mode) (fboundp 'font-lock-ensure))
        (font-lock-ensure (max (point-min) (- (point-max) (length s))) (point-max))))
    (when at-eob
      (with-current-buffer buf
        (goto-char (point-max))))))

(defun cursor-acp--chat-append-user (sess text)
  (cursor-acp--chat-insert sess (concat "\n\n> " (replace-regexp-in-string "\n" "\n> " text) "\n") t))

(defun cursor-acp--chat-open-assistant (sess)
  (unless cursor-acp--assistant-open
    (setq-local cursor-acp--assistant-open t)
    (with-current-buffer (cursor-acp--session-chat-buffer sess)
      (setq-local cursor-acp--assistant-start (copy-marker (point-max) nil)))
    (cursor-acp--chat-insert sess "\nACP> " t)))

(defun cursor-acp--hide-markup-region (beg end)
  (when (and (integerp beg) (integerp end) (< beg end))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (let ((inhibit-read-only t))
          ;; Clear previous render overlays in this region.
          (remove-overlays (point-min) (point-max) 'cursor-acp-md-hide t)
          ;; Tables: render Markdown pipe tables as aligned text (display overlay).
          (remove-overlays (point-min) (point-max) 'cursor-acp-table t)
          (goto-char (point-min))
          (while (re-search-forward "^\\s-*|.*|\\s-*$" nil t)
            (beginning-of-line)
            (let ((tbl-beg (point)))
              (while (and (not (eobp))
                          (looking-at "^\\s-*|.*|\\s-*$"))
                (forward-line 1))
              (let ((tbl-end (point)))
                (save-excursion
                  (goto-char tbl-beg)
                  (let (rows widths)
                    (while (< (point) tbl-end)
                      (let* ((line (buffer-substring-no-properties
                                    (line-beginning-position) (line-end-position)))
                             (cells (mapcar #'string-trim (split-string line "|" t))))
                        (setq cells (cl-remove-if (lambda (c) (string-empty-p (string-trim c))) cells))
                        (unless (or (null cells)
                                    (cl-every (lambda (c) (string-match-p "\\`[: -]+\\'" c)) cells))
                          (push cells rows)
                          (dotimes (i (length cells))
                            (let ((w (length (nth i cells))))
                              (if (nth i widths)
                                  (setf (nth i widths) (max (nth i widths) w))
                                (setq widths (append widths (list w))))))))
                      (forward-line 1))
                    (setq rows (nreverse rows))
                    (when (and rows widths)
                      (let* ((render
                              (mapconcat
                               (lambda (r)
                                 (mapconcat
                                  (lambda (i)
                                    (let* ((c (or (nth i r) ""))
                                           (w (nth i widths)))
                                      (format (format "%%-%ds" w) c)))
                                  (number-sequence 0 (1- (length widths)))
                                  "  "))
                               rows
                               "\n"))
                             (ov (make-overlay tbl-beg tbl-end)))
                        (overlay-put ov 'cursor-acp-table t)
                        (overlay-put ov 'display render)))))))))
          ;; Headings: hide leading #'s and following single space.
          (goto-char (point-min))
          (while (re-search-forward "^\\(#+\\)\\(\\s-+\\)" nil t)
            (let ((ov1 (make-overlay (match-beginning 1) (match-end 1)))
                  (ov2 (make-overlay (match-beginning 2) (match-end 2))))
              (overlay-put ov1 'cursor-acp-md-hide t)
              (overlay-put ov1 'display "")
              (overlay-put ov2 'cursor-acp-md-hide t)
              (overlay-put ov2 'display "")))
          ;; Bold/italic markers: **, __, *, _
          (goto-char (point-min))
          (while (re-search-forward "\\(\\*\\*\\|__\\|\\*\\|_\\)" nil t)
            (let ((ov (make-overlay (match-beginning 1) (match-end 1))))
              (overlay-put ov 'cursor-acp-md-hide t)
              (overlay-put ov 'display "")))
          ;; Inline code markers: `
          (goto-char (point-min))
          (while (re-search-forward "`" nil t)
            (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
              (overlay-put ov 'cursor-acp-md-hide t)
              (overlay-put ov 'display "")))
          ;; Fenced code blocks: hide ``` fences.
          (goto-char (point-min))
          (while (re-search-forward "^```.*$" nil t)
            (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
              (overlay-put ov 'cursor-acp-md-hide t)
              (overlay-put ov 'display "")))))))

(defun cursor-acp--assistant-append (sess txt)
  (setf (cursor-acp--session-assistant-frag sess)
        (concat (or (cursor-acp--session-assistant-frag sess) "") (or txt "")))
  (when (or (string-match-p "\n" (or txt ""))
            (>= (cursor-acp--count-words (cursor-acp--session-assistant-frag sess))
                cursor-acp-chat-flush-words))
    (cursor-acp--chat-insert sess (cursor-acp--session-assistant-frag sess) t)
    (setf (cursor-acp--session-assistant-frag sess) "")))

(defun cursor-acp--assistant-end-turn (sess)
  (unless (string-empty-p (or (cursor-acp--session-assistant-frag sess) ""))
    (cursor-acp--chat-insert sess (cursor-acp--session-assistant-frag sess) t)
    (setf (cursor-acp--session-assistant-frag sess) ""))
  (with-current-buffer (cursor-acp--session-chat-buffer sess)
    (when (markerp cursor-acp--assistant-start)
      (cursor-acp--hide-markup-region (marker-position cursor-acp--assistant-start) (point-max))))
  (setq-local cursor-acp--assistant-open nil)
  (cursor-acp--chat-insert sess "\n\n" t))

(define-derived-mode cursor-acp-info-mode special-mode "Cursor-ACP-Info"
  "Major mode for the Cursor ACP info buffer."
  (cursor-acp--enable-emulation-keys))

(defvar cursor-acp-info-mode-map
  (let ((map (copy-keymap special-mode-map)))
    (define-key map (kbd "C-c C-s") #'cursor-acp-start)
    (define-key map (kbd "C-c C-k") #'cursor-acp-stop)
    (define-key map (kbd "C-c C-r") #'cursor-acp-reset-layout)
    (define-key map (kbd "C-c C-l") #'cursor-acp-show-logs)
    (define-key map (kbd "C-c C-p") #'cursor-acp-focus-input)
    (define-key map (kbd "C-c C-m") #'cursor-acp-switch-mode)
    (define-key map (kbd "C-c C-M") #'cursor-acp-switch-model)
    (define-key map (kbd "C-c C-/") #'cursor-acp-run-command)
    (define-key map (kbd "TAB") #'cursor-acp-toggle-section)
    (define-key map (kbd "<tab>") #'cursor-acp-toggle-section)
    (define-key map (kbd "C-i") #'cursor-acp-toggle-section)
    map)
  "Keymap for `cursor-acp-info-mode'.")

(defun cursor-acp--render-info (sess)
  (let ((buf (cursor-acp--session-info-buffer sess)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (cursor-acp--status-line sess) "\n")
          (insert (make-string (min 80 (window-body-width nil t)) ?─) "\n\n")
          (cursor-acp--insert-section
           sess "Keys" (cursor-acp--keys-items) "(no keys?)")
          (insert "\n")
          (cursor-acp--insert-section
           sess "Modes" (cursor-acp--session-available-modes sess))
          (insert "\n")
          (cursor-acp--insert-section
           sess "Models" (cursor-acp--session-available-models sess)
           "(not advertised by ACP initialize)")
          (insert "\n")
          (cursor-acp--insert-section
           sess "Commands" (cursor-acp--session-available-commands sess)
           "(not advertised by ACP initialize)"))))))

(defun cursor-acp-show-info ()
  (interactive)
  (let ((sess (cursor-acp--ensure-session)))
    (cursor-acp--render-info sess)
    (pop-to-buffer (cursor-acp--session-info-buffer sess))
    (cursor-acp-info-mode)))

(define-derived-mode cursor-acp-input-mode text-mode "Cursor-ACP-Input"
  "Major mode for Cursor ACP input buffer."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (cursor-acp--enable-emulation-keys))

(defvar cursor-acp-input-mode-map
  (let ((map (copy-keymap text-mode-map)))
    (define-key map (kbd "RET") #'cursor-acp-send)
    (define-key map (kbd "C-j") #'newline)
    (define-key map (kbd "C-c C-p") (lambda () (interactive) (cursor-acp--input-focus (cursor-acp--ensure-session))))
    map)
  "Keymap for `cursor-acp-input-mode'.")

(defun cursor-acp--maybe-enable-markdown-font-lock ()
  (when (require 'markdown-mode nil t)
    (let (md-font-lock-defaults
          md-syntax-table
          md-syntax-propertize
          md-font-lock-extra-managed-props)
      (with-temp-buffer
        (markdown-mode)
        (setq md-font-lock-defaults font-lock-defaults)
        (setq md-syntax-table (copy-syntax-table (syntax-table)))
        (setq md-syntax-propertize syntax-propertize-function)
        (setq md-font-lock-extra-managed-props font-lock-extra-managed-props))
      (when md-font-lock-defaults
        (setq-local font-lock-defaults md-font-lock-defaults)
        (when md-font-lock-extra-managed-props
          (setq-local font-lock-extra-managed-props md-font-lock-extra-managed-props))
        (when md-syntax-table
          (set-syntax-table md-syntax-table))
        (when md-syntax-propertize
          (setq-local syntax-propertize-function md-syntax-propertize))
        (font-lock-mode 1)
        (jit-lock-mode 1)))))

(defun cursor-acp--maybe-hide-markdown-markup ()
  (when (and cursor-acp-markdown-hide-markup
             (featurep 'markdown-mode))
    (cond
     ((fboundp 'markdown-toggle-markup-hiding)
      (ignore-errors (markdown-toggle-markup-hiding)))
     ((fboundp 'markdown-hide-markup)
      (ignore-errors (markdown-hide-markup))))))

(defun cursor-acp-send ()
  (interactive)
  (let* ((sess (cursor-acp--ensure-session))
         (text (cursor-acp--input-text sess)))
    (when (string-empty-p (string-trim text))
      (user-error "Input is empty"))
    (cursor-acp--input-clear sess)
    (cursor-acp--chat-append-user sess text)
    (cursor-acp--chat-open-assistant sess)
    (cursor-acp--set-busy sess t)
    (let ((sid (cursor-acp--ensure-connected-session sess)))
      (cursor-acp--rpc-send
       sess "session/prompt"
       `((sessionId . ,sid)
         (prompt . [((type . "text") (text . ,text))]))))
    (cursor-acp--input-focus sess)))

(defun cursor-acp--rpc-next-id (sess)
  (let ((id (cursor-acp--session-next-id sess)))
    (setf (cursor-acp--session-next-id sess) (1+ id))
    id))

(defun cursor-acp--rpc-send (sess method params)
  (let* ((id (cursor-acp--rpc-next-id sess))
         (msg `((jsonrpc . "2.0") (id . ,id) (method . ,method) (params . ,params)))
         (json (cursor-acp--json-encode msg))
         (line (concat json "\n"))
         (proc (cursor-acp--session-process sess)))
    (cursor-acp--log sess "-->" json)
    (process-send-string proc line)
    id))

(defun cursor-acp--rpc-call (sess method params &optional timeout-seconds)
  (let* ((timeout (or timeout-seconds 20.0))
         (deadline (+ (float-time) timeout))
         (pending (cursor-acp--session-pending sess))
         (id (cursor-acp--rpc-send sess method params)))
    (puthash id (list :done nil :result nil :error nil) pending)
    (while (and (not (plist-get (gethash id pending) :done))
                (< (float-time) deadline)
                (process-live-p (cursor-acp--session-process sess)))
      (accept-process-output (cursor-acp--session-process sess) 0.05))
    (let* ((state (gethash id pending))
           (done (and state (plist-get state :done)))
           (err (and state (plist-get state :error)))
           (res (and state (plist-get state :result))))
      (when state (remhash id pending))
      (cond
       ((not done) (error "ACP RPC timeout: %s" method))
       (err (error "ACP RPC error for %s: %S" method err))
       (t res)))))

(defun cursor-acp--handle-message (sess msg)
  (let ((id (gethash "id" msg nil))
        (method (gethash "method" msg nil)))
    (cond
     (id
      (let ((state (gethash id (cursor-acp--session-pending sess))))
        (when state
          (if-let ((err (gethash "error" msg nil)))
              (progn
                (plist-put state :done t)
                (plist-put state :error err))
            (plist-put state :done t)
            (plist-put state :result (gethash "result" msg nil)))))
      (let* ((res (gethash "result" msg nil))
             (sr (and (hash-table-p res) (gethash "stopReason" res nil))))
        (when (string-equal sr "end_turn")
          (cursor-acp--set-busy sess nil)
          (cursor-acp--assistant-end-turn sess)
          (cursor-acp--render-info sess))))
     ((and method (string-equal method "session/update"))
      (let* ((params (gethash "params" msg nil))
             (update (and (hash-table-p params)
                          (or (gethash "update" params nil)
                              (gethash "sessionUpdate" params nil)))))
        (when (hash-table-p update)
          (cursor-acp--maybe-cache-commands-from-update sess update)
          (cursor-acp--maybe-cache-config-options-from-update sess update)
          (when-let ((mode (or (cursor-acp--ht-get update "mode")
                               (cursor-acp--ht-get update "modeId")
                               (cursor-acp--ht-get update "currentModeId"))))
            (when (stringp mode) (setf (cursor-acp--session-current-mode sess) mode)))
          (when-let ((model (or (cursor-acp--ht-get update "model")
                                (cursor-acp--ht-get update "modelId")
                                (cursor-acp--ht-get update "currentModelId"))))
            (when (stringp model) (setf (cursor-acp--session-current-model sess) model))))
        (with-current-buffer (cursor-acp--session-chat-buffer sess)
          (setq-local header-line-format (cursor-acp--status-line sess)))
        (if (and (hash-table-p update)
                 (string-equal (cursor-acp--ht-get update "sessionUpdate") "agent_message_chunk"))
            (let* ((content (cursor-acp--ht-get update "content"))
                   (txt (and (hash-table-p content) (cursor-acp--ht-get content "text"))))
              (when (stringp txt)
                (cursor-acp--chat-open-assistant sess)
                (cursor-acp--assistant-append sess txt)))
          (cursor-acp--render-info sess)))))))

(defun cursor-acp--ensure-connected-session (sess)
  (unless (and (cursor-acp--session-process sess)
               (process-live-p (cursor-acp--session-process sess))
               (cursor-acp--session-session-id sess))
    (cursor-acp-start))
  (cursor-acp--session-session-id sess))

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

(defun cursor-acp--config-option-select (sess opt)
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

(defun cursor-acp-switch-mode ()
  "Interactively switch ACP session mode."
  (interactive)
  (let* ((sess (cursor-acp--ensure-session))
         (opt (or (cursor-acp--config-option-by-category sess "mode")
                  (cursor-acp--config-option-by-id sess "mode"))))
    (cond
     (opt
      (let ((val (cursor-acp--config-option-select sess opt)))
        (cursor-acp--set-config-option sess "mode" val)))
     (t
      (let* ((sid (cursor-acp--ensure-connected-session sess))
             (modes (or (cursor-acp--session-available-modes sess) (cursor-acp--default-modes)))
             (alist
              (mapcar
               (lambda (m)
                 (let ((id (or (cursor-acp--ht-get m "id") (cursor-acp--ht-get m "name"))))
                   (cons (format "%s%s"
                                 id
                                 (if (string-equal id (cursor-acp--session-current-mode sess))
                                     " (current)"
                                   ""))
                         id)))
               (cl-remove-if-not #'hash-table-p modes)))
             (choice (completing-read "Mode: " (mapcar #'car alist) nil t))
             (mode-id (cdr (assoc choice alist))))
        (cursor-acp--rpc-call sess "session/set_mode" `((sessionId . ,sid) (modeId . ,mode-id)))
        (setf (cursor-acp--session-current-mode sess) mode-id)
        (with-current-buffer (cursor-acp--session-chat-buffer sess)
          (setq-local header-line-format (cursor-acp--status-line sess)))
        (cursor-acp--render-info sess))))))

(defun cursor-acp-switch-model ()
  "Interactively switch ACP session model."
  (interactive)
  (let* ((sess (cursor-acp--ensure-session))
         (opt (or (cursor-acp--config-option-by-category sess "model")
                  (cursor-acp--config-option-by-id sess "model"))))
    (unless opt
      (user-error "ACP did not advertise a model selector (configOptions category/id 'model')"))
    (let ((val (cursor-acp--config-option-select sess opt)))
      (cursor-acp--set-config-option sess "model" val)
      (setf (cursor-acp--session-current-model sess) val))))

(defun cursor-acp-run-command ()
  "Interactively run an advertised ACP slash command."
  (interactive)
  (let* ((sess (cursor-acp--ensure-session))
         (sid (cursor-acp--ensure-connected-session sess))
         (cmds (or (cursor-acp--session-available-commands sess) '()))
         (alist
          (mapcar
           (lambda (c)
             (let ((name (or (cursor-acp--ht-get c "name")
                             (cursor-acp--ht-get c "id")
                             (cursor-acp--ht-get c "command"))))
               (cons (format "/%s" name) c)))
           (cl-remove-if-not #'hash-table-p cmds)))
         (choice (completing-read "Command: " (mapcar #'car alist) nil t))
         (cmd (cdr (assoc choice alist)))
         (cmd-name (or (cursor-acp--ht-get cmd "name")
                       (cursor-acp--ht-get cmd "id")
                       (cursor-acp--ht-get cmd "command")))
         (input (cursor-acp--ht-get cmd "input"))
         (hint (and (hash-table-p input) (cursor-acp--ht-get input "hint")))
         (arg (when hint (read-string (format "%s: " hint)))))
    (cursor-acp--rpc-call
     sess "session/prompt"
     `((sessionId . ,sid)
       (prompt . [((type . "text")
                   (text . ,(string-trim (format "/%s %s" cmd-name (or arg "")))))])))))

(defun cursor-acp--process-filter (proc chunk)
  (let ((sess (and (cursor-acp--valid-session-p cursor-acp--current-session)
                   cursor-acp--current-session)))
    (when (and sess (eq proc (cursor-acp--session-process sess)))
      (setf (cursor-acp--session-stdout-acc sess)
            (concat (or (cursor-acp--session-stdout-acc sess) "") chunk))
      (let ((acc (cursor-acp--session-stdout-acc sess))
            (start 0)
            (len (length (cursor-acp--session-stdout-acc sess))))
        (while (and (< start len) (string-match "\n" acc start))
          (let* ((end (match-beginning 0))
                 (line (string-trim (substring acc start end))))
            (setq start (1+ end))
            (unless (string-empty-p line)
              (condition-case err
                  (progn
                    (cursor-acp--log sess "<--" line)
                    (cursor-acp--handle-message
                     sess (json-parse-string line :object-type 'hash-table :array-type 'list)))
                (error
                 (cursor-acp--log sess "<--parse-error" (format "%S\n%s" err line)))))))
        (setf (cursor-acp--session-stdout-acc sess) (substring acc start))))))

(defun cursor-acp--process-sentinel (proc event)
  (let ((sess (and (cursor-acp--valid-session-p cursor-acp--current-session)
                   cursor-acp--current-session)))
    (when (and sess (eq proc (cursor-acp--session-process sess)))
      (cursor-acp--log sess "process" (string-trim event))
      (cursor-acp--set-busy sess nil)
      (setf (cursor-acp--session-process sess) nil)
      (setf (cursor-acp--session-session-id sess) nil)
      (ignore-errors
        (with-current-buffer (cursor-acp--session-chat-buffer sess)
          (setq-local header-line-format (cursor-acp--status-line sess))))
      (ignore-errors (cursor-acp--render-info sess)))))

(defun cursor-acp--ensure-process (sess)
  (let ((proc (cursor-acp--session-process sess)))
    (when (and proc (process-live-p proc))
      (cl-return-from cursor-acp--ensure-process proc))
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
      (setf (cursor-acp--session-process sess) proc)
      (cursor-acp--log sess "process" "spawned")
      (with-current-buffer (cursor-acp--session-chat-buffer sess)
        (setq-local header-line-format (cursor-acp--status-line sess)))
      proc)))

(defun cursor-acp--handshake (sess)
  (let ((init-res
         (cursor-acp--rpc-call
          sess "initialize"
          `((protocolVersion . 1)
            (clientCapabilities . ((fs . ((readTextFile . :json-false)
                                          (writeTextFile . :json-false)))
                                   (terminal . :json-false)))
            (clientInfo . ((name . "emacs-cursor-acp") (version . "0.0.1")))))))
    (when (hash-table-p init-res)
      (cursor-acp--cache-capabilities sess init-res)))
  (cursor-acp--rpc-call sess "authenticate" '((methodId . "cursor_login")))
  (let ((res (cursor-acp--rpc-call
              sess "session/new"
              `((cwd . ,(expand-file-name default-directory))
                (mcpServers . [])))))
    (when (hash-table-p res)
      (setf (cursor-acp--session-session-id sess) (gethash "sessionId" res nil))
      (cursor-acp--cache-session-new sess res)
      (cursor-acp--render-info sess))))

(defun cursor-acp-start ()
  "Start ACP and create a session."
  (interactive)
  (let ((sess (cursor-acp--ensure-session)))
    (cursor-acp--ensure-process sess)
    (condition-case err
        (progn
          (cursor-acp--handshake sess)
          (with-current-buffer (cursor-acp--session-chat-buffer sess)
            (setq-local header-line-format (cursor-acp--status-line sess)))
          (cursor-acp--render-info sess))
      (error
       (cursor-acp--log sess "error" (format "%S" err))
       (with-current-buffer (cursor-acp--session-chat-buffer sess)
         (setq-local header-line-format (cursor-acp--status-line sess)))
       (cursor-acp--render-info sess)
       (signal (car err) (cdr err))))))

(defun cursor-acp-stop ()
  "Hard stop: kill `agent acp` and clear session state."
  (interactive)
  (let ((sess (cursor-acp--ensure-session)))
    (cursor-acp--set-busy sess nil)
    (when-let ((proc (cursor-acp--session-process sess)))
      (ignore-errors (delete-process proc)))
    (setf (cursor-acp--session-process sess) nil)
    (setf (cursor-acp--session-session-id sess) nil)
    (cursor-acp--log sess "process" "killed by user")
    (with-current-buffer (cursor-acp--session-chat-buffer sess)
      (setq-local header-line-format (cursor-acp--status-line sess)))
    (cursor-acp--render-info sess)))

(defun cursor-acp-show-logs ()
  "Show the ACP log buffer."
  (interactive)
  (pop-to-buffer (cursor-acp--session-log-buffer (cursor-acp--ensure-session))))

(defvar cursor-acp--global-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") #'cursor-acp-start)
    (define-key map (kbd "C-c C-k") #'cursor-acp-stop)
    (define-key map (kbd "C-c C-r") #'cursor-acp-reset-layout)
    (define-key map (kbd "C-c C-l") #'cursor-acp-show-logs)
    (define-key map (kbd "C-c C-i") #'cursor-acp-show-info)
    (define-key map (kbd "C-c C-p") #'cursor-acp-focus-input)
    (define-key map (kbd "C-c C-m") #'cursor-acp-switch-mode)
    (define-key map (kbd "C-c C-M") #'cursor-acp-switch-model)
    (define-key map (kbd "C-c C-/") #'cursor-acp-run-command)
    map)
  "Keymap shared across Cursor ACP buffers.")

(define-minor-mode cursor-acp--global-keys-mode
  "Minor mode for shared Cursor ACP keybindings."
  :init-value nil
  :lighter ""
  :keymap cursor-acp--global-keys-mode-map)

(defvar-local cursor-acp--emulation-keys-on nil)

(defun cursor-acp--enable-emulation-keys ()
  (setq-local cursor-acp--emulation-keys-on t)
  (setq-local emulation-mode-map-alists
              (cons `((cursor-acp--emulation-keys-on . ,cursor-acp--global-keys-mode-map))
                    emulation-mode-map-alists)))

(defvar cursor-acp-chat-mode-map
  (let ((map (copy-keymap text-mode-map)))
    (define-key map (kbd "C-c C-s") #'cursor-acp-start)
    (define-key map (kbd "C-c C-k") #'cursor-acp-stop)
    (define-key map (kbd "C-c C-r") #'cursor-acp-reset-layout)
    (define-key map (kbd "C-c C-l") #'cursor-acp-show-logs)
    (define-key map (kbd "C-c C-i") #'cursor-acp-show-info)
    (define-key map (kbd "C-c C-p") #'cursor-acp-focus-input)
    (define-key map (kbd "C-c C-m") #'cursor-acp-switch-mode)
    (define-key map (kbd "C-c C-M") #'cursor-acp-switch-model)
    (define-key map (kbd "C-c C-/") #'cursor-acp-run-command)
    map))

(define-derived-mode cursor-acp-chat-mode fundamental-mode "Cursor-ACP"
  "Major mode for Cursor ACP buffer."
  (setq-local truncate-lines t)
  (setq-local word-wrap t)
  (setq-local truncate-lines nil)
  (cursor-acp--enable-emulation-keys)
  (when (require 'markdown-mode nil t)
    (markdown-mode))
  (when (bound-and-true-p view-mode)
    (view-mode -1))
  (when (bound-and-true-p read-only-mode)
    (read-only-mode -1))
  (setq-local buffer-read-only nil)
  (let ((sess (cursor-acp--ensure-session)))
    (setq-local header-line-format (cursor-acp--status-line sess))
    (when (zerop (buffer-size))
      (let ((inhibit-read-only t))
        (insert (propertize "Cursor ACP Chat\n" 'face 'cursor-acp-header-face))
        (insert (make-string (min 80 (window-body-width nil t)) ?─) "\n")))))

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key)
    (evil-define-key '(insert) cursor-acp-input-mode-map
      (kbd "RET") #'cursor-acp-send
      (kbd "C-j") #'newline)
    (evil-define-key '(normal motion) cursor-acp-chat-mode-map
      (kbd "C-c C-i") #'cursor-acp-show-info
      (kbd "C-c C-p") #'cursor-acp-focus-input)
    ;; In the chat buffer we want normal typing in insert-state.
    (evil-define-key '(insert) cursor-acp-chat-mode-map
      (kbd "C-c C-i") #'cursor-acp-show-info
      (kbd "C-c C-p") #'cursor-acp-focus-input)))

(defun cursor-acp ()
  "Open the Cursor ACP buffer."
  (interactive)
  (let ((sess (cursor-acp--ensure-session)))
    (let ((cur (current-buffer)))
      (unless (cursor-acp--acp-buffer-p sess cur)
        (setf (cursor-acp--session-main-buffer sess) cur)))
    (let ((chat (cursor-acp--session-chat-buffer sess))
          (input (cursor-acp--session-input-buffer sess)))
      (pop-to-buffer chat)
      (when (require 'markdown-mode nil t)
        (markdown-mode))
      (setq-local header-line-format (cursor-acp--status-line sess))
      (unless (get-buffer-window input)
        (let ((win (selected-window)))
          (select-window (split-window win 5 'below))
          (switch-to-buffer input)))
      (with-current-buffer input
        (cursor-acp-input-mode))
      (cursor-acp--ensure-input-window-height sess)
      (cursor-acp--input-focus sess))))

(provide 'cursor-acp)
;;; cursor-acp.el ends here

