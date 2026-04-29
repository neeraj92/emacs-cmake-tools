;;; cursor-acp-ui.el --- Cursor ACP UI -*- lexical-binding: t; -*-

(require 'cursor-acp-core)
(autoload 'markdown-mode "markdown-mode" nil t)

(defvar-local cursor-acp--assistant-open nil)
(defvar-local cursor-acp--assistant-start nil)
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
   "C-c C-x  cancel turn"
   "C-c C-a  reprompt permission"
   "C-c C-r  reset layout"
   "C-c C-l  show logs"
   "C-c C-i  show info"
   "C-c C-p  focus input"
   "RET      send (in input)"
   "C-j      newline (in input)"
   "TAB/C-i  toggle section"))

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

(defun cursor-acp--chat-eob-p ()
  (>= (point) (max (point-min) (- (point-max) 2))))

(defun cursor-acp--chat-insert (sess s &optional read-only face)
  (let ((buf (cursor-acp--session-chat-buffer sess)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (let ((start (point)))
          (insert s)
          (when face
            (add-face-text-property start (point) face 'append))
          (when read-only
            (put-text-property start (point) 'read-only t)))))
    (with-current-buffer buf
      (when (and (bound-and-true-p font-lock-mode) (fboundp 'font-lock-ensure))
        (font-lock-ensure (max (point-min) (- (point-max) (length s))) (point-max))))
    (with-current-buffer buf
      (goto-char (point-max)))))

(defun cursor-acp--chat-insert-prefix (sess face)
  (let ((buf (cursor-acp--session-chat-buffer sess)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (let ((beg (point)))
          (insert "> ")
          (let ((ov (make-overlay beg (point))))
            (overlay-put ov 'cursor-acp-prefix t)
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'display (propertize "❯ " 'face face))
            (overlay-put ov 'face nil))
          (put-text-property beg (point) 'read-only t))))))

(defun cursor-acp--chat-append-user (sess text)
  (cursor-acp--chat-insert sess "\n\n" t)
  (dolist (line (split-string (or text "") "\n" nil))
    (cursor-acp--chat-insert-prefix sess 'cursor-acp-user-prefix-face)
    (cursor-acp--chat-insert sess line t)
    (cursor-acp--chat-insert sess "\n" t)))

(defun cursor-acp--chat-open-assistant (sess)
  (unless cursor-acp--assistant-open
    (setq-local cursor-acp--assistant-open t)
    (with-current-buffer (cursor-acp--session-chat-buffer sess)
      (setq-local cursor-acp--assistant-start (copy-marker (point-max) nil)))
    (cursor-acp--chat-insert sess "\n" t)
    (cursor-acp--chat-insert-prefix sess 'cursor-acp-agent-prefix-face)))

(defun cursor-acp--hide-markup-region (beg end)
  (when (and (integerp beg) (integerp end) (< beg end))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (let ((inhibit-read-only t))
          (remove-overlays (point-min) (point-max) 'cursor-acp-md-hide t)
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
          (goto-char (point-min))
          (while (re-search-forward "^\\(#+\\)\\(\\s-+\\)" nil t)
            (let ((ov1 (make-overlay (match-beginning 1) (match-end 1)))
                  (ov2 (make-overlay (match-beginning 2) (match-end 2))))
              (overlay-put ov1 'cursor-acp-md-hide t)
              (overlay-put ov1 'display "")
              (overlay-put ov2 'cursor-acp-md-hide t)
              (overlay-put ov2 'display "")))
          (goto-char (point-min))
          (while (re-search-forward "\\(\\*\\*\\|__\\|\\*\\|_\\)" nil t)
            (let ((ov (make-overlay (match-beginning 1) (match-end 1))))
              (overlay-put ov 'cursor-acp-md-hide t)
              (overlay-put ov 'display "")))
          (goto-char (point-min))
          (while (re-search-forward "`" nil t)
            (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
              (overlay-put ov 'cursor-acp-md-hide t)
              (overlay-put ov 'display "")))
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
    (cursor-acp--chat-insert sess (cursor-acp--session-assistant-frag sess) t 'cursor-acp-agent-text-face)
    (setf (cursor-acp--session-assistant-frag sess) "")))

(defun cursor-acp--assistant-end-turn (sess)
  (unless (string-empty-p (or (cursor-acp--session-assistant-frag sess) ""))
    (cursor-acp--chat-insert sess (cursor-acp--session-assistant-frag sess) t 'cursor-acp-agent-text-face)
    (setf (cursor-acp--session-assistant-frag sess) ""))
  (with-current-buffer (cursor-acp--session-chat-buffer sess)
    (when (markerp cursor-acp--assistant-start)
      (cursor-acp--hide-markup-region (marker-position cursor-acp--assistant-start) (point-max))))
  (setq-local cursor-acp--assistant-open nil)
  (cursor-acp--chat-insert sess "\n\n" t))

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
         (summary (cursor-acp--permission-request-summary params))
         (reason (cursor-acp--permission-request-reasons params)))
    (with-current-buffer (cursor-acp--session-chat-buffer sess)
      (let ((end (point-max)))
        (cursor-acp--hide-markup-region (point-min) end)))
    (cursor-acp--chat-insert sess "\n" t)
    (cursor-acp--chat-insert sess (propertize "Permission requested" 'face 'bold) t)
    (when (and (stringp summary) (not (string-empty-p summary)))
      (cursor-acp--chat-insert sess (concat " " summary) t))
    (cursor-acp--chat-insert sess "\n" t)
    (when (and (stringp title) (not (string-empty-p title)))
      (cursor-acp--chat-insert sess (concat "```sh\n" title "\n```\n") t))
    (when (and (stringp reason) (not (string-empty-p reason)))
      (cursor-acp--chat-insert sess (propertize "Reason" 'face 'bold) t)
      (cursor-acp--chat-insert sess ":\n" t)
      (cursor-acp--chat-insert sess (concat reason "\n") t))))

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
      (cursor-acp--chat-insert sess (format "[permission selected] optionId=%s\n" selected) t)
      `((outcome . "selected") (optionId . ,selected)))))

(defun cursor-acp--render-tool-call-raw-output (sess tool-call-id raw-output)
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
                             v
                           (condition-case _
                               (json-serialize v :pretty t :null-object :null :false-object :json-false)
                             (error (format "%S" v))))))
               (push (format "%s\n```text\n%s\n```\n" (propertize key 'face 'bold) txt) sections)))))
       raw-output)
      (when sections
        (cursor-acp--chat-insert sess (concat "\n" (mapconcat #'identity (nreverse sections) "")) t))))
   (t
    (let ((txt (if (stringp raw-output)
                   raw-output
                 (condition-case _
                     (json-serialize raw-output :pretty t :null-object :null :false-object :json-false)
                   (error (format "%S" raw-output))))))
      (unless (and (stringp txt) (string-empty-p (string-trim txt)))
        (cursor-acp--chat-insert
         sess
         (format "\n```text\n%s\n```\n" txt)
         t))))))

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

(defvar cursor-acp--global-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") #'cursor-acp-start)
    (define-key map (kbd "C-c C-k") #'cursor-acp-stop)
    (define-key map (kbd "C-c C-x") #'cursor-acp-cancel-turn)
    (define-key map (kbd "C-c C-a") #'cursor-acp-reprompt-permission)
    (define-key map (kbd "C-c C-r") #'cursor-acp-reset-layout)
    (define-key map (kbd "C-c C-l") #'cursor-acp-show-logs)
    (define-key map (kbd "C-c C-i") #'cursor-acp-show-info)
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
    (define-key map (kbd "C-c C-k") #'cursor-acp-stop)
    (define-key map (kbd "C-c C-x") #'cursor-acp-cancel-turn)
    (define-key map (kbd "C-c C-a") #'cursor-acp-reprompt-permission)
    (define-key map (kbd "C-c C-r") #'cursor-acp-reset-layout)
    (define-key map (kbd "C-c C-l") #'cursor-acp-show-logs)
    (define-key map (kbd "C-c C-p") #'cursor-acp-focus-input)
    (define-key map (kbd "C-c C-m") #'cursor-acp-switch-mode)
    (define-key map (kbd "C-c C-M") #'cursor-acp-switch-model)
    (define-key map (kbd "C-c C-/") #'cursor-acp-run-command)
    (define-key map (kbd "TAB") #'cursor-acp-toggle-section)
    (define-key map (kbd "<tab>") #'cursor-acp-toggle-section)
    (define-key map (kbd "C-i") #'cursor-acp-toggle-section)
    map))

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
    map))

(defvar cursor-acp-chat-mode-map
  (let ((map (copy-keymap text-mode-map)))
    (define-key map (kbd "C-c C-s") #'cursor-acp-start)
    (define-key map (kbd "C-c C-k") #'cursor-acp-stop)
    (define-key map (kbd "C-c C-x") #'cursor-acp-cancel-turn)
    (define-key map (kbd "C-c C-a") #'cursor-acp-reprompt-permission)
    (define-key map (kbd "C-c C-r") #'cursor-acp-reset-layout)
    (define-key map (kbd "C-c C-l") #'cursor-acp-show-logs)
    (define-key map (kbd "C-c C-i") #'cursor-acp-show-info)
    (define-key map (kbd "C-c C-p") #'cursor-acp-focus-input)
    (define-key map (kbd "C-c C-m") #'cursor-acp-switch-mode)
    (define-key map (kbd "C-c C-M") #'cursor-acp-switch-model)
    (define-key map (kbd "C-c C-/") #'cursor-acp-run-command)
    map))

(define-derived-mode cursor-acp-chat-mode markdown-mode "Cursor-ACP"
  "Major mode for Cursor ACP buffer."
  (when (boundp 'markdown-mode-map)
    (set-keymap-parent cursor-acp-chat-mode-map markdown-mode-map))
  (setq-local truncate-lines t)
  (setq-local word-wrap t)
  (setq-local truncate-lines nil)
  (cursor-acp--enable-emulation-keys)
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
    (evil-define-key '(insert) cursor-acp-chat-mode-map
      (kbd "C-c C-i") #'cursor-acp-show-info
      (kbd "C-c C-p") #'cursor-acp-focus-input)))

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

(defun cursor-acp--open-ui (sess)
  (let ((cur (current-buffer)))
    (unless (cursor-acp--acp-buffer-p sess cur)
      (setf (cursor-acp--session-main-buffer sess) cur)))
  (let ((chat (cursor-acp--session-chat-buffer sess))
        (input (cursor-acp--session-input-buffer sess)))
    (pop-to-buffer chat)
    (with-current-buffer chat
      (cursor-acp-chat-mode))
    (setq-local header-line-format (cursor-acp--status-line sess))
    (unless (get-buffer-window input)
      (let ((win (selected-window)))
        (select-window (split-window win 5 'below))
        (switch-to-buffer input)))
    (with-current-buffer input
      (cursor-acp-input-mode))
    (cursor-acp--ensure-input-window-height sess)
    (cursor-acp--input-focus sess)))

(provide 'cursor-acp-ui)
;;; cursor-acp-ui.el ends here
