;;; cursor-acp-ui.el --- Cursor ACP UI -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'map)
(require 'shell-maker)
(unless (require 'markdown-overlays nil t)
  (error "markdown-overlays not found; update shell-maker"))
(require 'cursor-acp-core)

(autoload 'markdown-mode "markdown-mode" nil t)
(autoload 'gfm-mode "markdown-mode" nil t)

(defvar-local cursor-acp--emulation-keys-on nil)

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

(defun cursor-acp--insert-section (sess name items &optional empty-note)
  (let* ((expanded (cursor-acp--ui-expanded-p sess name))
         (marker (if expanded "➖" "➕")))
    (insert (format "%s %s\n" marker name))
    (when expanded
      (if items
          (cursor-acp--insert-items items)
        (insert "  " (or empty-note "(none)") "\n")))))

(defun cursor-acp--section-at-point ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^[➖➕] \\(Keys\\|Modes\\|Models\\|Commands\\|Transcript\\)\\s-*$")
      (match-string 1))))

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
   "C-c C-n  new session"
   "C-c C-k  hard stop"
   "C-c C-x  cancel turn"
   "C-c C-a  reprompt permission"
   "C-c C-b  reprompt ask question"
   "C-c C-r  reset layout"
   "C-c C-v  review file edits (ediff)"
   "C-c C-w  switch session"
   "C-c C-l  show logs"
   "C-c C-i  show info"
   "C-c C-p  focus shell"
   "RET      send (at comint prompt)"
   "TAB/C-i  toggle section (in info buffer)"))

;;; Shell-maker config and mode

(defvar cursor-acp--shell-config
  (make-shell-maker-config
   :name "Cursor-ACP"
   :execute-command
   (lambda (input shell)
     (message "Received the call here in the submit function")
     (let ((sess (or (cursor-acp--session-for-buffer (current-buffer))
                     (cursor-acp--active-session))))
       (if sess
	   (message "Session is fine")
	 (message "Session is nil"))
       (when sess
         (setf (cursor-acp--session-shell-write-output sess) (map-elt shell :write-output))
         (setf (cursor-acp--session-shell-finish-output sess) (map-elt shell :finish-output))
         (cursor-acp--set-busy sess t)
         (let ((sid (cursor-acp--ensure-connected-session sess)))
           (cursor-acp--set-active-session-id sid)
           (cursor-acp--rpc-send-async
            sess "session/prompt"
            `((sessionId . ,sid)
              (prompt . [((type . "text") (text . ,input))]))
            sid)))
       ))
   :on-command-finished
   (lambda (_input _output _success)
     (when cursor-acp-markdown-hide-markup
       (markdown-overlays-put))))
  "shell-maker config for cursor-acp sessions.")

(shell-maker-define-major-mode cursor-acp--shell-config)
(define-key cursor-acp-shell-mode-map (kbd "C-<up>") #'scroll-up-line)
(define-key cursor-acp-shell-mode-map (kbd "C-<down>") #'scroll-down-line)

(add-hook 'cursor-acp-shell-mode-hook
          (lambda ()
            (cursor-acp--enable-emulation-keys)
            (add-hook 'completion-at-point-functions #'cursor-acp--at-file-capf nil t)
            (add-hook 'post-self-insert-hook #'cursor-acp--maybe-trigger-at-file-completion nil t)))

;;; Window layout helpers

(defun cursor-acp--pane-window-p (win)
  (and (window-live-p win)
       (window-parameter win 'cursor-acp-pane)))

(defun cursor-acp--pane-windows (&optional frame)
  (let ((f (or frame (selected-frame))))
    (cl-remove-if-not
     #'cursor-acp--pane-window-p
     (window-list f 'no-minibuf))))

(defun cursor-acp--delete-pane-windows (&optional frame)
  (dolist (w (cursor-acp--pane-windows frame))
    (when (window-live-p w)
      (ignore-errors (delete-window w)))))

(defun cursor-acp--pane-allow-resize (win)
  (when (window-live-p win)
    (set-window-parameter win 'window-size-fixed nil)
    (set-window-parameter win 'window-preserve-size nil)))

(defun cursor-acp--pane-snap-width (win width)
  (when (and (window-live-p win) (integerp width) (> width 0))
    (cursor-acp--pane-allow-resize win)
    (let* ((cur (window-total-width win))
           (delta (- width cur)))
      (when (/= delta 0)
        (ignore-errors (window-resize win delta t t))))))

(defun cursor-acp--ensure-plan-window-height (sess)
  (let* ((buf (cursor-acp--session-plan-buffer sess))
         (win (and (buffer-live-p buf) (get-buffer-window buf t))))
    (when (window-live-p win)
      (let* ((frame-lines (frame-height (window-frame win)))
             (target (max window-min-height
                          (max 3 (floor (* (max 10 frame-lines) 0.10))))))
        (cond
         ((fboundp 'set-window-text-height)
          (ignore-errors (set-window-text-height win target)))
         (t
          (let* ((cur (window-body-height win))
                 (delta (- target cur)))
            (when (/= delta 0)
              (ignore-errors (window-resize win delta))))))))))

(defun cursor-acp--ensure-shell-pane (sess &optional force-width)
  (let* ((buf (cursor-acp--session-shell-buffer sess))
         (plan (cursor-acp--session-plan-buffer sess))
         (shell-win
          (or (cl-find-if
               #'cursor-acp--pane-window-p
               (get-buffer-window-list buf nil t))
              (display-buffer-in-side-window
               buf
               `((side . right)
                 (slot . 0)
                 (window-width . ,cursor-acp-pane-width)
                 (window-parameters . ((cursor-acp-pane . t))))))))
    (when (window-live-p shell-win)
      (set-window-parameter shell-win 'cursor-acp-pane t)
      (set-window-dedicated-p shell-win t)
      (cursor-acp--pane-allow-resize shell-win)
      (when force-width
        (cursor-acp--pane-snap-width shell-win cursor-acp-pane-width))
      (when (and (buffer-live-p plan) (cursor-acp--session-display-plan-buffer sess))
        (let ((plan-win
               (or (cl-find-if
                    #'cursor-acp--pane-window-p
                    (get-buffer-window-list plan nil t))
                   (display-buffer-in-side-window
                    plan
                    `((side . right)
                      (slot . 1)
                      (window-width . ,cursor-acp-pane-width)
                      (window-height . 0.10)
                      (window-parameters . ((cursor-acp-pane . t))))))))
          (when (window-live-p plan-win)
            (set-window-parameter plan-win 'cursor-acp-pane t)
            (set-window-dedicated-p plan-win t)
            (cursor-acp--pane-allow-resize plan-win)
            (cursor-acp--ensure-plan-window-height sess))))
      shell-win)))

;;; Plan buffer rendering

(defun cursor-acp--format-plan-todo-line (entry)
  (let* ((content (and (hash-table-p entry) (cursor-acp--ht-get entry "content")))
         (status0 (and (hash-table-p entry) (cursor-acp--ht-get entry "status")))
         (status (when (stringp status0) (downcase status0)))
         (label (string-trim (format "%s" (or content "")))))
    (cond
     ((string-equal status "completed")
      (format "- [x] %s" label))
     ((or (string-equal status "cancelled") (string-equal status "canceled"))
      (format "- [ ] ~~%s~~" (replace-regexp-in-string "~" "-" label)))
     (t
      (format "- [ ] %s" label)))))

(defun cursor-acp--plan-todos-section-text (entries)
  (with-temp-buffer
    (insert "## Todos\n\n")
    (let ((items (cursor-acp--normalize-items entries)))
      (if (not items)
          (insert "- (none)\n")
        (dolist (e items)
          (insert (cursor-acp--format-plan-todo-line e) "\n"))))
    (buffer-string)))

(defun cursor-acp--plan-buffer-after-insert-markdown-view ()
  (when (and cursor-acp-markdown-hide-markup (boundp 'markdown-hide-markup))
    (setq-local markdown-hide-markup t)
    (ignore-errors
      (when (fboundp 'font-lock-flush)
        (font-lock-flush (point-min) (point-max)))
      (when (fboundp 'font-lock-ensure)
        (font-lock-ensure (point-min) (point-max))))))

(defun cursor-acp--render-plan-entries (sess entries)
  (let ((buf (or (cursor-acp--session-plan-buffer sess)
                 (let ((b (get-buffer-create (format "*cursor-acp-plan: %s*"
                                                     (cursor-acp--session-buffer-title sess)))))
                   (setf (cursor-acp--session-plan-buffer sess) b)
                   b))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (fboundp 'gfm-mode)
            (gfm-mode)
          (markdown-mode))
        (insert (cursor-acp--plan-todos-section-text entries))
        (cursor-acp--plan-buffer-after-insert-markdown-view)
        (goto-char (point-min))
        (setq-local buffer-read-only t)))
    (setf (cursor-acp--session-display-plan-buffer sess) t)
    (cursor-acp--ensure-shell-pane sess)
    (cursor-acp--ensure-plan-window-height sess)))

;;; Shell buffer management

(defun cursor-acp--ensure-shell-buffer (sess)
  "Ensure SESS has a live shell-maker buffer; create it if needed."
  (let ((buf (cursor-acp--session-shell-buffer sess)))
    (unless (buffer-live-p buf)
      (message "Starting the shell maker")
      (setq buf (shell-maker-start
                 cursor-acp--shell-config
                 t    ; no-focus — we control display ourselves
                 nil  ; no welcome message
                 nil  ; no new-session (we control buffer names)
                 (cursor-acp--shell-buffer-name sess)))
      (setf (cursor-acp--session-shell-buffer sess) buf)
      (with-current-buffer buf
        (setq-local header-line-format (cursor-acp--chat-header-line sess))))
    buf))

(defun cursor-acp--shell-focus (sess)
  (let ((buf (cursor-acp--session-shell-buffer sess)))
    (when (buffer-live-p buf)
      (let ((win (or (get-buffer-window buf t)
                     (cursor-acp--ensure-shell-pane sess))))
        (when (window-live-p win)
          (select-window win)))
      (with-current-buffer buf
        (goto-char (point-max)))
      (when (and (featurep 'evil) (fboundp 'evil-insert-state))
        (evil-insert-state)))))

;;; Chat output functions (write to the shell buffer via shell-maker)

(defun cursor-acp--chat-insert (sess s &optional _read-only _face)
  (when (and (stringp s) (cursor-acp--valid-session-p sess))
    (let ((write-fn (cursor-acp--session-shell-write-output sess))
          (buf (cursor-acp--session-shell-buffer sess)))
      (cond
       (write-fn
        (funcall write-fn s))
       ((buffer-live-p buf)
        (with-current-buffer buf
          (shell-maker-write-output :config cursor-acp--shell-config :output s)))))))

(defun cursor-acp--chat-clear (sess)
  (let ((buf (cursor-acp--session-shell-buffer sess)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (shell-maker-clear-buffer)))))

(defun cursor-acp--chat-append-user (_sess _text)
  ;; comint echoes user input automatically; no manual append needed
  nil)

(defun cursor-acp--chat-open-assistant (_sess)
  nil)

(defun cursor-acp--assistant-flush-frag (_sess)
  nil)

(defun cursor-acp--assistant-append (sess txt)
  (when-let ((fn (cursor-acp--session-shell-write-output sess)))
    (when (stringp txt)
      (funcall fn txt))))

(defun cursor-acp--assistant-end-turn (sess)
  (let ((finish-fn (cursor-acp--session-shell-finish-output sess)))
    (when finish-fn
      (funcall finish-fn t)
      (setf (cursor-acp--session-shell-write-output sess) nil)
      (setf (cursor-acp--session-shell-finish-output sess) nil)))
  (cursor-acp--session-transcript-save sess))

(defun cursor-acp--schedule-preview (_sess)
  nil)

(defun cursor-acp--shell-at-prompt-p ()
  (let ((preg (shell-maker-prompt-regexp cursor-acp--shell-config)))
    (and (stringp preg) (not (string-empty-p preg))
         (save-excursion
           (goto-char (point-max))
           (forward-line 0)
           (looking-at preg)))))

(defun cursor-acp--shell-finalize-display (sess)
  "Ensure SESS shell buffer has a comint prompt and markdown overlays."
  (when-let ((buf (cursor-acp--session-shell-buffer sess)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (eq major-mode (shell-maker-major-mode cursor-acp--shell-config))
          (unless (cursor-acp--shell-at-prompt-p)
            (shell-maker-finish-output :config cursor-acp--shell-config :success t))
          (when cursor-acp-markdown-hide-markup
            (markdown-overlays-put)))))))

(defvar cursor-acp--shell-finalize-timer nil)

(defun cursor-acp--schedule-shell-finalize (sess &optional delay)
  "After DELAY seconds of idle, finalize SESS shell display (prompt + overlays)."
  (when (cursor-acp--valid-session-p sess)
    (when cursor-acp--shell-finalize-timer
      (cancel-timer cursor-acp--shell-finalize-timer))
    (setq cursor-acp--shell-finalize-timer
          (run-with-idle-timer (or delay 0.3) nil
                               #'cursor-acp--shell-finalize-display sess))))

(defun cursor-acp--session-transcript-restore (sess)
  "Restore SESS shell buffer from the saved transcript file, if readable."
  (when-let ((path (cursor-acp--session-transcript-file sess)))
    (when (and (file-readable-p path) (cursor-acp--valid-session-p sess))
      (cursor-acp--ensure-shell-buffer sess)
      (let ((history
             (with-temp-buffer
               (insert-file-contents path)
               (shell-maker--extract-history
                (shell-maker-prompt-regexp cursor-acp--shell-config)
                :propertized nil))))
        (when history
          (with-current-buffer (cursor-acp--session-shell-buffer sess)
            (shell-maker-restore-session-from-transcript history))
          t)))))

;;; Info buffer rendering

(defun cursor-acp--render-info (sess)
  (let ((buf (cursor-acp--session-info-buffer sess)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (cursor-acp--status-line sess) "\n")
          (insert (make-string (min 80 (window-body-width nil t)) ?-) "\n\n")
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

;;; Permission and ask-question UI (write inline to shell buffer)

(defun cursor-acp--truncate (s maxlen)
  (let* ((s0 (if (stringp s) s (format "%s" (or s ""))))
         (n (max 0 (or maxlen 0))))
    (if (<= (length s0) n)
        s0
      (if (<= n 3)
          (substring s0 0 n)
        (concat (substring s0 0 (- n 3)) "...")))))

(defun cursor-acp--permission-request-summary (params)
  (let* ((tool-call (and (hash-table-p params) (cursor-acp--ht-get params "toolCall")))
         (title (and (hash-table-p tool-call) (cursor-acp--ht-get tool-call "title")))
         (kind (and (hash-table-p tool-call) (cursor-acp--ht-get tool-call "kind"))))
    (string-trim
     (format "%s%s"
             (if (stringp kind) (format "kind=%s" kind) "")
             (if (stringp title) (format " title=%s" title) "")))))

(defun cursor-acp--permission-request-reasons (params)
  (let* ((tool-call (and (hash-table-p params) (cursor-acp--ht-get params "toolCall")))
         (content-items (cursor-acp--normalize-items
                         (and (hash-table-p tool-call) (cursor-acp--ht-get tool-call "content"))))
         (texts '()))
    (dolist (it content-items)
      (when (hash-table-p it)
        (let* ((node (cursor-acp--ht-get it "content"))
               (txt (and (hash-table-p node) (cursor-acp--ht-get node "text"))))
          (when (stringp txt)
            (push txt texts)))))
    (string-join (nreverse texts) "\n")))

(defun cursor-acp--render-permission-request (sess params)
  (let* ((tool-call (and (hash-table-p params) (cursor-acp--ht-get params "toolCall")))
         (title (and (hash-table-p tool-call) (cursor-acp--ht-get tool-call "title")))
         (title-short (and title (cursor-acp--truncate title 75)))
         (summary (cursor-acp--permission-request-summary params))
         (reason (cursor-acp--permission-request-reasons params))
         (reason-short (and reason (cursor-acp--truncate reason 100))))
    (cursor-acp--chat-insert sess "\n" t)
    (cursor-acp--chat-insert sess (propertize "🚨 Permission requested" 'face '(bold warning)) t)
    (when (and (stringp summary) (not (string-empty-p summary)))
      (cursor-acp--chat-insert sess (concat " " summary) t))
    (cursor-acp--chat-insert sess "\n" t)
    (when (and (stringp title-short) (not (string-empty-p title-short)))
      (cursor-acp--chat-insert sess (concat "```sh\n" title-short "\n```\n") t))
    (when (and (stringp reason-short) (not (string-empty-p reason-short)))
      (cursor-acp--chat-insert sess (propertize "💡 Reason" 'face '(bold font-lock-keyword-face)) t)
      (cursor-acp--chat-insert sess ":\n" t)
      (cursor-acp--chat-insert sess (concat reason-short "\n") t))))

(defun cursor-acp--permission-prompt-select-option (options)
  (let* ((pairs (cl-loop for o in options
                         for i from 1
                         for nm = (or (cursor-acp--ht-get o "name")
                                      (cursor-acp--ht-get o "kind")
                                      (cursor-acp--ht-get o "optionId")
                                      (format "option-%d" i))
                         for oid = (cursor-acp--ht-get o "optionId")
                         collect (list i nm oid)))
         (prompt
          (format
           "ACP permission %s: "
           (mapconcat (lambda (p) (format "[%d] %s" (nth 0 p) (nth 1 p))) pairs "  ")))
         (choices (mapcar (lambda (p) (+ ?0 (nth 0 p))) pairs))
         (ch (read-char-choice prompt choices))
         (idx (- ch ?0))
         (sel (cl-find-if (lambda (p) (= (nth 0 p) idx)) pairs)))
    (nth 2 sel)))

(defun cursor-acp--prompt-permission-decision (sess params)
  (let* ((opts (cursor-acp--normalize-items (and (hash-table-p params) (cursor-acp--ht-get params "options"))))
         (options (cl-remove-if-not #'hash-table-p opts))
         (_summary (cursor-acp--permission-request-summary params)))
    (cursor-acp--render-permission-request sess params)
    (unless options
      (user-error "Permission request did not include options"))
    (let ((selected (cursor-acp--permission-prompt-select-option options)))
      (cursor-acp--chat-insert
       sess
       (propertize (format "✅ [permission selected] optionId=%s\n" selected) 'face '(bold success))
       t)
      `((outcome . "selected") (optionId . ,selected)))))

(defun cursor-acp--render-ask-question-title (sess params &optional skip-banner)
  (unless skip-banner
    (let ((title (and (hash-table-p params) (cursor-acp--ht-get params "title"))))
      (cursor-acp--chat-insert sess "\n" t)
      (cursor-acp--chat-insert sess (propertize "❓ Question" 'face '(bold font-lock-keyword-face)) t)
      (when (and (stringp title) (not (string-empty-p (string-trim title))))
        (cursor-acp--chat-insert sess (concat " " (string-trim title)) t))
      (cursor-acp--chat-insert sess "\n" t))))

(defun cursor-acp--render-ask-question-question (sess q n total)
  (let* ((opts (cl-remove-if-not
                #'hash-table-p
                (cursor-acp--normalize-items (cursor-acp--ht-get q "options"))))
         (pairs
          (cl-loop for o in opts
                   for i from 1
                   for oid = (cursor-acp--ht-get o "id")
                   for lbl = (cursor-acp--ht-get o "label")
                   for id0 = (string-trim (format "%s" (or oid "")))
                   for lb0 = (string-trim (format "%s" (or lbl id0 "")))
                   collect (list i lb0 id0))))
    (when (null pairs)
      (user-error "Question has no options"))
    (cursor-acp--chat-insert sess "\n" t)
    (cursor-acp--chat-insert sess (format "-- Question %d/%d" n total) t)
    (cursor-acp--chat-insert sess "\n" t)
    (let ((qprompt (cursor-acp--ht-get q "prompt")))
      (when (and (stringp qprompt) (not (string-empty-p (string-trim qprompt))))
        (cursor-acp--chat-insert sess (concat (string-trim qprompt) "\n") t)))
    (dolist (p pairs)
      (cursor-acp--chat-insert sess (format "%d. %s\n" (nth 0 p) (nth 1 p)) t))
    pairs))

(defun cursor-acp--truncate-string (s maxlen)
  (let ((s0 (if (stringp s) s (format "%s" (or s ""))))
        (n (max 0 (or maxlen 0))))
    (if (<= (length s0) n)
        s0
      (if (<= n 3) (substring s0 0 n) (concat (substring s0 0 (- n 3)) "...")))))

(defun cursor-acp--ask-question-select-option-ids (prompt-prefix pairs allow-multiple)
  (unless pairs
    (user-error "Question has no options"))
  (let ((nopts (length pairs))
        (piece-width 30))
    (if allow-multiple
        (let* ((prompt-pieces
                (mapconcat
                 (lambda (p)
                   (format "[%d] %s" (nth 0 p) (cursor-acp--truncate-string (nth 1 p) piece-width)))
                 pairs "  "))
               (s (read-string (format "%s%s (comma-separated numbers): "
                                       prompt-prefix prompt-pieces)))
               (raw-parts (split-string s "[,;[:space:]]+" t))
               (seen (make-hash-table :test #'eql))
               out)
          (unless raw-parts
            (user-error "Select at least one option"))
          (dolist (part raw-parts)
            (let* ((trim (string-trim part))
                   idx)
              (unless (string-empty-p trim)
                (unless (string-match-p "\\`[0-9]+\\'" trim)
                  (user-error "Invalid option number: %s" trim))
                (setq idx (string-to-number trim))
                (unless (cl-find-if (lambda (pr) (= (car pr) idx)) pairs)
                  (user-error "Unknown option number: %d" idx))
                (unless (gethash idx seen)
                  (puthash idx t seen)
                  (push (nth 2 (cl-find-if (lambda (pr) (= (car pr) idx)) pairs)) out)))))
          (unless out
            (user-error "Select at least one option"))
          (nreverse out))
      (if (<= nopts 9)
          (let* ((prompt-pieces
                  (mapconcat
                   (lambda (p)
                     (format "[%d] %s" (nth 0 p) (cursor-acp--truncate-string (nth 1 p) piece-width)))
                   pairs "  "))
                 (prompt (format "%s%s: " prompt-prefix prompt-pieces))
                 (choices (mapcar (lambda (p) (+ ?0 (car p))) pairs))
                 (ch (read-char-choice prompt choices))
                 (idx (- ch ?0))
                 (sel (cl-find-if (lambda (pr) (= (car pr) idx)) pairs)))
            (list (nth 2 sel)))
        (let* ((prompt-pieces
                (mapconcat
                 (lambda (p)
                   (format "[%d] %s" (car p) (cursor-acp--truncate-string (nth 1 p) piece-width)))
                 pairs "\n"))
               (s (read-string (format "%s\n%sOption number (1-%d): "
                                       prompt-pieces prompt-prefix nopts)))
               (trim (string-trim s)))
          (unless (string-match-p "\\`[0-9]+\\'" trim)
            (user-error "Invalid option number"))
          (let* ((idx (string-to-number trim))
                 (sel (cl-find-if (lambda (pr) (= (car pr) idx)) pairs)))
            (unless sel
              (user-error "Unknown option number: %d" idx))
            (list (nth 2 sel))))))))

(defun cursor-acp--prompt-ask-question-decision (sess params &optional skip-title)
  (let* ((questions (cursor-acp--normalize-items
                     (and (hash-table-p params) (cursor-acp--ht-get params "questions"))))
         (qhts (cl-remove-if-not #'hash-table-p questions))
         (total (length qhts))
         (rows '()))
    (when (null qhts)
      (user-error "Ask question request did not include questions"))
    (cursor-acp--render-ask-question-title sess params skip-title)
    (cl-loop for q in qhts
             for n from 1
             do (let* ((qid0 (string-trim (format "%s" (or (cursor-acp--ht-get q "id") ""))))
                       (multi (and (cursor-acp--ht-get q "allowMultiple") t))
                       (pairs (cursor-acp--render-ask-question-question sess q n total))
                       (picked-ids
                        (cursor-acp--ask-question-select-option-ids
                         (format "ACP question [%d/%d] " n total) pairs multi))
                       (picked-labels
                        (mapcar
                         (lambda (oid)
                           (nth 1 (cl-find-if (lambda (p) (string-equal oid (nth 2 p))) pairs)))
                         picked-ids)))
                  (cursor-acp--chat-insert
                   sess
                   (propertize (format "✅ Selected (q %d/%d): %s\n"
                                       n total (string-join picked-labels ", "))
                               'face '(bold success))
                   t)
                  (push `((questionId . ,qid0) (selectedOptionIds . ,(vconcat picked-ids)))
                        rows)))
    (let ((answers (vconcat (nreverse rows))))
      (cursor-acp--chat-insert
       sess
       (propertize "✅ [question answered]\n" 'face '(bold success))
       t)
      `((outcome . "answered") (answers . ,answers)))))

;;; Tool call and diff rendering

(defun cursor-acp--read-tool-kind-p (tool-kind)
  (let ((k (and (stringp tool-kind) (downcase (string-trim tool-kind)))))
    (and k (member k '("read" "read_file" "readfile" "read file" "fs/read_text_file")))))

(defun cursor-acp--render-tool-call-raw-output (sess tool-call-id raw-output &optional tool-kind)
  (ignore tool-call-id)
  (cond
   ((hash-table-p raw-output)
    (let (sections)
      (maphash
       (lambda (k v)
         (let* ((skip
                 (or (equal v 0)
                     (and (stringp v) (string-empty-p (string-trim v)))
                     (and (hash-table-p v) (= (hash-table-count v) 0))
                     (and (vectorp v) (= (length v) 0))
                     (and (listp v) (null v)))))
           (unless skip
             (let* ((key (format "%s" k))
                    (txt (if (stringp v)
                             (if (cursor-acp--read-tool-kind-p tool-kind)
                                 (cursor-acp--truncate-string v 100)
                               v)
                           (condition-case _
                               (json-serialize v :pretty t :null-object :null :false-object :json-false)
                             (error (format "%S" v))))))
               (push (format "%s\n```text\n%s\n```\n" (propertize key 'face 'bold) txt) sections)))))
       raw-output)
      (when sections
        (cursor-acp--chat-insert sess (concat "\n" (mapconcat #'identity (nreverse sections) "")) t))))
   (t
    (let ((txt (if (stringp raw-output)
                   (if (cursor-acp--read-tool-kind-p tool-kind)
                       (cursor-acp--truncate-string raw-output 100)
                     raw-output)
                 (condition-case _
                     (json-serialize raw-output :pretty t :null-object :null :false-object :json-false)
                   (error (format "%S" raw-output))))))
      (unless (and (stringp txt) (string-empty-p (string-trim txt)))
        (cursor-acp--chat-insert
         sess
         (format "\n```text\n%s\n```\n" txt)
         t))))))

(defun cursor-acp--render-tool-call-completed (sess tool-call-id title raw-output &optional tool-kind)
  (cursor-acp--assistant-flush-frag sess)
  (when (and (stringp title) (not (string-empty-p (string-trim title))))
    (cursor-acp--chat-insert
     sess
     (concat "\n" (propertize (format "🔧 %s" title) 'face '(bold font-lock-function-name-face)) "\n")
     t))
  (cursor-acp--render-tool-call-raw-output sess tool-call-id raw-output tool-kind))

(defcustom cursor-acp-diff-preview-lines 20
  "Maximum number of lines to display for diff previews in the chat buffer."
  :type 'integer
  :group 'cursor-acp)

(defun cursor-acp--diff-preview (text)
  (let* ((lines (split-string (or text "") "\n" nil))
         (n (max 0 (min (or cursor-acp-diff-preview-lines 20) (length lines))))
         (shown (cl-subseq lines 0 n)))
    (cons (if shown (concat (string-join shown "\n") "\n") "")
          (- (length lines) n))))

(defun cursor-acp--diff-run-unified (sess path old-text new-text)
  (let* ((sid (or (and (cursor-acp--valid-session-p sess)
                       (cursor-acp--session-session-id sess))
                  "no-session"))
         (base (format "cursor-acp-%s-" (replace-regexp-in-string "[^A-Za-z0-9_.-]" "_" sid)))
         (old-file (make-temp-file base nil ".old"))
         (new-file (make-temp-file base nil ".new")))
    (unwind-protect
        (progn
          (with-temp-file old-file
            (insert (or old-text "")))
          (with-temp-file new-file
            (insert (or new-text "")))
          (with-temp-buffer
            (let ((code (call-process
                         "diff" nil t nil
                         "-u"
                         "--label" (format "a/%s" (or path "old"))
                         "--label" (format "b/%s" (or path "new"))
                         old-file new-file)))
              (cond
               ((or (eq code 0) (eq code 1))
                (buffer-string))
               (t
                (format "diff failed (exit=%s)\n" code))))))
      (ignore-errors (delete-file old-file))
      (ignore-errors (delete-file new-file)))))

(defun cursor-acp--render-diff-block (sess path old-text new-text)
  (cursor-acp--assistant-flush-frag sess)
  (let* ((p (or path "(unknown path)"))
         (diff (cursor-acp--diff-run-unified sess p (or old-text "") (or new-text "")))
         (preview (cursor-acp--diff-preview diff))
         (txt (car preview))
         (remaining (cdr preview)))
    (cursor-acp--chat-insert
     sess
     (concat "\n\n**🔍 Diff: " p "**\n\n```diff\n" txt "```\n")
     t)
    (when (> remaining 0)
      (cursor-acp--chat-insert sess (format "_... (%d more lines)_\n" remaining) t))))

(defun cursor-acp--render-write-text-file (sess path)
  (cursor-acp--assistant-flush-frag sess)
  (cursor-acp--chat-insert
   sess
   (concat "\n\n**📝 Wrote: " (or path "(unknown path)") "**\n")
   t))

;;; Major modes

(defvar cursor-acp--global-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") #'cursor-acp-start)
    (define-key map (kbd "C-c C-n") #'cursor-acp-new-session)
    (define-key map (kbd "C-c C-k") #'cursor-acp-stop)
    (define-key map (kbd "C-c C-x") #'cursor-acp-cancel-turn)
    (define-key map (kbd "C-c C-a") #'cursor-acp-reprompt-permission)
    (define-key map (kbd "C-c C-b") #'cursor-acp-reprompt-ask-question)
    (define-key map (kbd "C-c C-r") #'cursor-acp-reset-layout)
    (define-key map (kbd "C-c C-v") #'cursor-acp-review)
    (define-key map (kbd "C-c C-w") #'cursor-acp-switch-session)
    (define-key map (kbd "C-c C-l") #'cursor-acp-show-logs)
    (define-key map (kbd "C-c C-i") #'cursor-acp-cycle-mode)
    (define-key map (kbd "C-c C-d") #'cursor-acp-show-info)
    (define-key map (kbd "C-c C-p") #'cursor-acp-focus-input)
    (define-key map (kbd "C-c C-m") #'cursor-acp-switch-mode)
    (define-key map (kbd "C-c C-M") #'cursor-acp-switch-model)
    (define-key map (kbd "C-c C-/") #'cursor-acp-run-command)
    map))

(defun cursor-acp--enable-emulation-keys ()
  (setq-local cursor-acp--emulation-keys-on t)
  (setq-local emulation-mode-map-alists
              (cons `((cursor-acp--emulation-keys-on . ,cursor-acp--global-keys-mode-map))
                    emulation-mode-map-alists)))

(define-derived-mode cursor-acp-info-mode special-mode "Cursor-ACP-Info"
  "Major mode for the Cursor ACP info buffer."
  (cursor-acp--enable-emulation-keys))

(defvar cursor-acp-info-mode-map
  (let ((map (copy-keymap special-mode-map)))
    (define-key map (kbd "C-c C-s") #'cursor-acp-start)
    (define-key map (kbd "C-c C-n") #'cursor-acp-new-session)
    (define-key map (kbd "C-c C-k") #'cursor-acp-stop)
    (define-key map (kbd "C-c C-x") #'cursor-acp-cancel-turn)
    (define-key map (kbd "C-c C-a") #'cursor-acp-reprompt-permission)
    (define-key map (kbd "C-c C-b") #'cursor-acp-reprompt-ask-question)
    (define-key map (kbd "C-c C-r") #'cursor-acp-reset-layout)
    (define-key map (kbd "C-c C-v") #'cursor-acp-review)
    (define-key map (kbd "C-c C-l") #'cursor-acp-show-logs)
    (define-key map (kbd "C-c C-i") #'cursor-acp-cycle-mode)
    (define-key map (kbd "C-c C-d") #'cursor-acp-show-info)
    (define-key map (kbd "C-c C-p") #'cursor-acp-focus-input)
    (define-key map (kbd "C-c C-m") #'cursor-acp-switch-mode)
    (define-key map (kbd "C-c C-M") #'cursor-acp-switch-model)
    (define-key map (kbd "C-c C-/") #'cursor-acp-run-command)
    (define-key map (kbd "TAB") #'cursor-acp-toggle-section)
    (define-key map (kbd "<tab>") #'cursor-acp-toggle-section)
    (define-key map (kbd "C-i") #'cursor-acp-toggle-section)
    map))

;;; @-file completion (shared between shell and info buffers)

(defun cursor-acp--path-has-hidden-component-p (path)
  (let* ((parts (split-string (directory-file-name path) "/" t)))
    (catch 'hidden
      (dolist (p parts)
        (when (and (stringp p) (not (string-empty-p p))
                   (eq (aref p 0) ?.))
          (throw 'hidden t)))
      nil)))

(defun cursor-acp--workspace-files (root)
  (let* ((r (file-name-as-directory (expand-file-name root)))
         (all (ignore-errors (directory-files-recursively r ".*" nil nil)))
         (files
          (cl-remove-if
           (lambda (p)
             (or (not (stringp p))
                 (cursor-acp--path-has-hidden-component-p (file-relative-name p r))))
           all)))
    (sort (mapcar (lambda (p) (file-relative-name p r)) files) #'string-lessp)))

(defun cursor-acp--at-file-exit (str status)
  (when (eq status 'finished)
    (let* ((end (point))
           (start (- end (length str)))
           (at-pos (1- start)))
      (when (and (>= at-pos (point-min))
                 (eq (char-after at-pos) ?@))
        (delete-region at-pos (1+ at-pos))))))

(defun cursor-acp--at-file-capf ()
  "Completion for workspace files after '@' in ACP shell buffers."
  (let* ((sess (or (cursor-acp--session-for-buffer (current-buffer))
                   (cursor-acp--active-session)
                   (cursor-acp--ensure-session)))
         (end (point)))
    (save-excursion
      (skip-chars-backward "A-Za-z0-9_./-")
      (let* ((beg (point))
             (at-pos (1- beg)))
        (when (and (>= at-pos (point-min))
                   (eq (char-after at-pos) ?@))
          (list beg end
                (cursor-acp--workspace-files (cursor-acp--workspace-root sess))
                :exclusive 'no
                :exit-function #'cursor-acp--at-file-exit))))))

(defun cursor-acp--maybe-trigger-at-file-completion ()
  (when (and (derived-mode-p 'cursor-acp-shell-mode)
             (eq last-command-event ?@))
    (completion-at-point)))

;;; Buffer identity helpers

(defun cursor-acp--acp-buffer-p (sess buf)
  (and (buffer-live-p buf)
       (memq buf
             (list (cursor-acp--session-shell-buffer sess)
                   (cursor-acp--session-info-buffer sess)
                   (cursor-acp--session-log-buffer sess)))))

(defun cursor-acp--preferred-main-buffer ()
  (let* ((sess (cursor-acp--ensure-session))
         (shell (cursor-acp--session-shell-buffer sess))
         (info (cursor-acp--session-info-buffer sess))
         (log (cursor-acp--session-log-buffer sess))
         (cur (window-buffer (selected-window))))
    (if (memq cur (list shell info log))
        (or (cl-find-if
             (lambda (b) (and (buffer-live-p b) (not (memq b (list shell info log)))))
             (buffer-list))
            (other-buffer cur t))
      cur)))

;;; Open/close UI

(defun cursor-acp--open-ui (sess &optional force-width)
  (message "Loading up the cursor acp ui")
  (let ((cur (current-buffer)))
    (unless (cursor-acp--acp-buffer-p sess cur)
      (setf (cursor-acp--session-main-buffer sess) cur)))
  (cursor-acp--ensure-shell-buffer sess)
  (let* ((shell-buf (cursor-acp--session-shell-buffer sess))
         (shell-win (cursor-acp--ensure-shell-pane sess force-width)))
    (when (window-live-p shell-win)
      (set-window-buffer shell-win shell-buf))
    (cursor-acp--shell-focus sess)))

(provide 'cursor-acp-ui)
;;; cursor-acp-ui.el ends here
