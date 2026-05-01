;;; cursor-acp-ui.el --- Cursor ACP UI -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cursor-acp-core)
(autoload 'markdown-mode "markdown-mode" nil t)
(autoload 'gfm-mode "markdown-mode" nil t)

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

(defun cursor-acp--ensure-pane (sess &optional force-width)
  (let* ((chat (cursor-acp--session-chat-buffer sess))
         (input (cursor-acp--session-input-buffer sess))
         (plan (cursor-acp--session-plan-buffer sess))
         (chat-win
          (or (cl-find-if
               #'cursor-acp--pane-window-p
               (get-buffer-window-list chat nil t))
              (display-buffer-in-side-window
               chat
               `((side . right)
                 (slot . 0)
                 (window-width . ,cursor-acp-pane-width)
                 (window-parameters . ((cursor-acp-pane . t))))))))
    (when (window-live-p chat-win)
      (set-window-parameter chat-win 'cursor-acp-pane t)
      (set-window-dedicated-p chat-win t)
      (cursor-acp--pane-allow-resize chat-win)
      (when force-width
        (cursor-acp--pane-snap-width chat-win cursor-acp-pane-width))
      (let ((input-win
             (or (cl-find-if
                  #'cursor-acp--pane-window-p
                  (get-buffer-window-list input nil t))
                 (display-buffer-in-side-window
                  input
                  `((side . right)
                    (slot . ,(if (buffer-live-p plan) 2 1))
                    (window-width . ,cursor-acp-pane-width)
                    (window-height . 0.10)
                    (window-parameters . ((cursor-acp-pane . t))))))))
        (when (window-live-p input-win)
          (set-window-parameter input-win 'cursor-acp-pane t)
          (set-window-dedicated-p input-win t)
          (cursor-acp--pane-allow-resize input-win)
          (when (buffer-live-p plan)
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
          (cursor-acp--ensure-input-window-height sess)
          (cons chat-win input-win))))))

(defun cursor-acp--format-plan-todo-line (entry)
  (let* ((content (and (hash-table-p entry) (cursor-acp--ht-get entry "content")))
         (status0 (and (hash-table-p entry) (cursor-acp--ht-get entry "status")))
         (status (when (stringp status0) (downcase status0)))
         (label (string-trim (format "%s" (or content "")))))
    (cond
     ((string-equal status "completed")
      (format "- [x] %s" label))
     ((or (string-equal status "cancelled") (string-equal status "canceled"))
      (format "- [ ] ~~%s~~" (replace-regexp-in-string "~" "∼" label)))
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
    (cursor-acp--ensure-pane sess)
    (cursor-acp--ensure-plan-window-height sess)))

(defun cursor-acp--input-focus (sess)
  (let ((buf (cursor-acp--session-input-buffer sess)))
    (when (buffer-live-p buf)
      (let ((win (or (get-buffer-window buf t)
                     (cdr (cursor-acp--ensure-pane sess)))))
        (when (window-live-p win)
          (select-window win)))
      (cursor-acp--ensure-input-window-height sess)
      (goto-char (point-max))
      (when (and (featurep 'evil) (fboundp 'evil-insert-state))
        (evil-insert-state)))))

(defun cursor-acp--chat-eob-p ()
  (>= (point) (max (point-min) (- (point-max) 2))))

(defun cursor-acp--chat-scroll-to-end (buf)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (goto-char (point-max))
      (dolist (win (get-buffer-window-list buf nil t))
        (when (window-live-p win)
          (set-window-point win (point-max)))))))

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
            (put-text-property start (point) 'read-only t))
          (cons start (point)))))
    (with-current-buffer buf
      (when (and (bound-and-true-p font-lock-mode) (fboundp 'font-lock-ensure))
        (font-lock-ensure (max (point-min) (- (point-max) (length s))) (point-max))))
    (cursor-acp--chat-scroll-to-end buf)))

(defun cursor-acp--chat-clear (sess)
  (with-current-buffer (cursor-acp--session-chat-buffer sess)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (setq-local cursor-acp--assistant-open nil)
    (setq-local cursor-acp--assistant-start nil)))

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
  (with-current-buffer (cursor-acp--session-chat-buffer sess)
    (unless cursor-acp--assistant-open
      (setq-local cursor-acp--assistant-open t)
      (setq-local cursor-acp--assistant-start (copy-marker (point-max) nil))
      (cursor-acp--chat-insert sess "\n" t)
      (cursor-acp--chat-insert-prefix sess 'cursor-acp-agent-prefix-face))))

(defun cursor-acp--in-diff-p (pos)
  (get-text-property pos 'cursor-acp-diff))

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
                (unless (or (cursor-acp--in-diff-p tbl-beg)
                            (cursor-acp--in-diff-p (max (point-min) (1- tbl-end))))
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
                          (overlay-put ov 'display render))))))))))
        (goto-char (point-min))
        (while (re-search-forward "^\\(#+\\)\\(\\s-+\\)" nil t)
          (unless (cursor-acp--in-diff-p (match-beginning 0))
            (let ((ov1 (make-overlay (match-beginning 1) (match-end 1)))
                  (ov2 (make-overlay (match-beginning 2) (match-end 2))))
              (overlay-put ov1 'cursor-acp-md-hide t)
              (overlay-put ov1 'display "")
              (overlay-put ov2 'cursor-acp-md-hide t)
              (overlay-put ov2 'display ""))))
        (goto-char (point-min))
        (while (re-search-forward "\\(\\*\\*\\|__\\|\\*\\|_\\)" nil t)
          (unless (cursor-acp--in-diff-p (match-beginning 0))
            (let ((ov (make-overlay (match-beginning 1) (match-end 1))))
              (overlay-put ov 'cursor-acp-md-hide t)
              (overlay-put ov 'display ""))))
        (goto-char (point-min))
        (while (re-search-forward "`" nil t)
          (unless (cursor-acp--in-diff-p (match-beginning 0))
            (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
              (overlay-put ov 'cursor-acp-md-hide t)
              (overlay-put ov 'display ""))))
        (goto-char (point-min))
        (while (re-search-forward "^```.*$" nil t)
          (unless (cursor-acp--in-diff-p (match-beginning 0))
            (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
              (overlay-put ov 'cursor-acp-md-hide t)
              (overlay-put ov 'display ""))))))))

(defun cursor-acp--assistant-append (sess txt)
  (setf (cursor-acp--session-assistant-frag sess)
        (concat (or (cursor-acp--session-assistant-frag sess) "") (or txt "")))
  (when (or (string-match-p "\n" (or txt ""))
            (>= (cursor-acp--count-words (cursor-acp--session-assistant-frag sess))
                cursor-acp-chat-flush-words))
    (let* ((frag (cursor-acp--session-assistant-frag sess))
           (range (cursor-acp--chat-insert sess frag t 'cursor-acp-agent-text-face)))
      (when (and cursor-acp-markdown-hide-markup (consp range))
        (with-current-buffer (cursor-acp--session-chat-buffer sess)
          (cursor-acp--hide-markup-region (car range) (cdr range)))))
    (setf (cursor-acp--session-assistant-frag sess) "")))

(defun cursor-acp--assistant-end-turn (sess)
  (unless (string-empty-p (or (cursor-acp--session-assistant-frag sess) ""))
    (cursor-acp--chat-insert sess (cursor-acp--session-assistant-frag sess) t 'cursor-acp-agent-text-face)
    (setf (cursor-acp--session-assistant-frag sess) ""))
  (with-current-buffer (cursor-acp--session-chat-buffer sess)
    (when (markerp cursor-acp--assistant-start)
      (cursor-acp--hide-markup-region (marker-position cursor-acp--assistant-start) (point-max))))
  (with-current-buffer (cursor-acp--session-chat-buffer sess)
    (setq-local cursor-acp--assistant-open nil))
  (cursor-acp--chat-insert sess "\n\n" t)
  (cursor-acp--session-transcript-save sess))

(defun cursor-acp--assistant-flush-frag (sess)
  "Flush any buffered assistant text without closing the assistant block."
  (unless (string-empty-p (or (cursor-acp--session-assistant-frag sess) ""))
    (cursor-acp--chat-insert sess (cursor-acp--session-assistant-frag sess) t 'cursor-acp-agent-text-face)
    (setf (cursor-acp--session-assistant-frag sess) "")))

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
    (with-current-buffer (cursor-acp--session-chat-buffer sess)
      (let ((end (point-max)))
        (cursor-acp--hide-markup-region (point-min) end)))
    (cursor-acp--chat-insert sess "\n" t)
    (cursor-acp--chat-insert sess (propertize "⚠ Permission requested" 'face '(bold warning)) t)
    (when (and (stringp summary) (not (string-empty-p summary)))
      (cursor-acp--chat-insert sess (concat " " summary) t))
    (cursor-acp--chat-insert sess "\n" t)
    (when (and (stringp title-short) (not (string-empty-p title-short)))
      (cursor-acp--chat-insert sess (concat "```sh\n" title-short "\n```\n") t))
    (when (and (stringp reason-short) (not (string-empty-p reason-short)))
      (cursor-acp--chat-insert sess (propertize "ℹ Reason" 'face '(bold font-lock-keyword-face)) t)
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
  (with-current-buffer (cursor-acp--session-chat-buffer sess)
    (let ((end (point-max)))
      (cursor-acp--hide-markup-region (point-min) end)))
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
    (let ((beg (with-current-buffer (cursor-acp--session-chat-buffer sess)
                 (goto-char (point-max))
                 (point))))
      (cursor-acp--chat-insert sess "\n" t)
      (cursor-acp--chat-insert
       sess
       (propertize (format "— Question %d/%d" n total) 'face '(bold font-lock-keyword-face))
       t)
      (cursor-acp--chat-insert sess "\n" t)
      (let ((qprompt (cursor-acp--ht-get q "prompt")))
        (when (and (stringp qprompt) (not (string-empty-p (string-trim qprompt))))
          (cursor-acp--chat-insert sess (concat (string-trim qprompt) "\n") t)))
      (dolist (p pairs)
        (cursor-acp--chat-insert sess (format "%d. %s\n" (nth 0 p) (nth 1 p)) t))
      (with-current-buffer (cursor-acp--session-chat-buffer sess)
        (cursor-acp--hide-markup-region beg (point-max))))
    pairs))

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

(defun cursor-acp--truncate-string (s maxlen)
  (let ((s0 (if (stringp s) s (format "%s" (or s ""))))
        (n (max 0 (or maxlen 0))))
    (if (<= (length s0) n)
        s0
      (if (<= n 3) (substring s0 0 n) (concat (substring s0 0 (- n 3)) "...")))))

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
     (concat "\n" (propertize (format "🛠 %s" title) 'face '(bold font-lock-function-name-face)) "\n")
     t))
  (cursor-acp--render-tool-call-raw-output sess tool-call-id raw-output tool-kind))

(defun cursor-acp--diff-added-face ()
  (cond
   ((facep 'magit-diff-added) 'magit-diff-added)
   ((facep 'diff-added) 'diff-added)
   (t 'success)))

(defun cursor-acp--diff-removed-face ()
  (cond
   ((facep 'magit-diff-removed) 'magit-diff-removed)
   ((facep 'diff-removed) 'diff-removed)
   (t 'error)))

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

(defun cursor-acp--apply-diff-faces (beg end)
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (let* ((lb (line-beginning-position))
             (le (min (line-end-position) end)))
        (when (< lb le)
          (cond
           ((looking-at-p "^\\+\\+\\+\\|^---\\|^@@")
            (add-face-text-property lb le 'bold 'append))
           ((looking-at-p "^\\+")
            (add-face-text-property lb le (cursor-acp--diff-added-face) 'append))
           ((looking-at-p "^-")
            (add-face-text-property lb le (cursor-acp--diff-removed-face) 'append))))
        (forward-line 1)))))

(defun cursor-acp--render-diff-block (sess path old-text new-text)
  (cursor-acp--assistant-flush-frag sess)
  (let* ((p (or path "(unknown path)"))
         (diff (cursor-acp--diff-run-unified sess p (or old-text "") (or new-text "")))
         (preview (cursor-acp--diff-preview diff))
         (txt (car preview))
         (remaining (cdr preview)))
    (let* ((chat (cursor-acp--session-chat-buffer sess))
           (w (or (and (buffer-live-p chat)
                       (get-buffer-window chat t)
                       (window-body-width (get-buffer-window chat t) t))
                  80))
           (sep (concat (make-string (max 10 (min 120 w)) ?─) "\n")))
      (with-current-buffer chat
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (let ((block-beg (point)))
            (insert "\n" sep)
            (insert (propertize (format "🔍 Diff: %s" p) 'face '(bold font-lock-keyword-face)) "\n")
            (insert sep)
            (insert "```diff\n")
            (insert txt)
            (insert "```\n")
            (when (> remaining 0)
              (insert (propertize (format "… (%d more lines)\n" remaining) 'face 'shadow)))
            (insert sep)
            (put-text-property block-beg (point) 'read-only t)
            (when (and (bound-and-true-p font-lock-mode) (fboundp 'font-lock-ensure))
              (font-lock-ensure block-beg (point)))
            (cursor-acp--hide-markup-region block-beg (point))))))))

(defun cursor-acp--render-write-text-file (sess path)
  (cursor-acp--assistant-flush-frag sess)
  (cursor-acp--chat-insert
   sess
   (concat "\n" (propertize (format "✍ Wrote: %s" (or path "(unknown path)"))
                            'face '(bold font-lock-keyword-face))
           "\n")
   t))

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

(define-derived-mode cursor-acp-input-mode text-mode "Cursor-ACP-Input"
  "Major mode for Cursor ACP input buffer."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (cursor-acp--enable-emulation-keys)
  (add-hook 'completion-at-point-functions #'cursor-acp--at-file-capf nil t)
  (add-hook 'post-self-insert-hook #'cursor-acp--maybe-trigger-at-file-completion nil t))

(defun cursor-acp--space (&optional n)
  (interactive "p")
  (let ((n (or n 1)))
    (if (and (eq major-mode 'cursor-acp-input-mode)
             (bound-and-true-p completion-in-region-mode)
             (eq (char-before) ?@))
        (progn
          (completion-in-region-mode -1)
          (self-insert-command n))
      (self-insert-command n))))

(defvar cursor-acp-input-mode-map
  (let ((map (copy-keymap text-mode-map)))
    (define-key map (kbd "RET") #'cursor-acp-send)
    (define-key map (kbd "C-j") #'newline)
    (define-key map (kbd "SPC") #'cursor-acp--space)
    (define-key map (kbd "C-c C-p") (lambda () (interactive) (cursor-acp--input-focus (cursor-acp--ensure-session))))
    map))

(defvar cursor-acp-chat-mode-map
  (let ((map (copy-keymap text-mode-map)))
    (define-key map (kbd "C-c C-s") #'cursor-acp-start)
    (define-key map (kbd "C-c C-k") #'cursor-acp-stop)
    (define-key map (kbd "C-c C-x") #'cursor-acp-cancel-turn)
    (define-key map (kbd "C-c C-a") #'cursor-acp-reprompt-permission)
    (define-key map (kbd "C-c C-b") #'cursor-acp-reprompt-ask-question)
    (define-key map (kbd "C-c C-r") #'cursor-acp-reset-layout)
    (define-key map (kbd "C-c C-l") #'cursor-acp-show-logs)
    (define-key map (kbd "C-c C-i") #'cursor-acp-cycle-mode)
    (define-key map (kbd "C-c C-d") #'cursor-acp-show-info)
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
    (setq-local header-line-format (cursor-acp--chat-header-line sess))
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
      (kbd "C-c C-i") #'cursor-acp-cycle-mode
      (kbd "C-c C-d") #'cursor-acp-show-info
      (kbd "C-c C-p") #'cursor-acp-focus-input)
    (evil-define-key '(insert) cursor-acp-chat-mode-map
      (kbd "C-c C-i") #'cursor-acp-cycle-mode
      (kbd "C-c C-d") #'cursor-acp-show-info
      (kbd "C-c C-p") #'cursor-acp-focus-input)))

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
  "Completion for workspace files after '@' in ACP input buffers."
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
  (when (and (eq major-mode 'cursor-acp-input-mode)
             (eq last-command-event ?@))
    (completion-at-point)))

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

(defun cursor-acp--open-ui (sess &optional force-width)
  (let ((cur (current-buffer)))
    (unless (cursor-acp--acp-buffer-p sess cur)
      (setf (cursor-acp--session-main-buffer sess) cur)))
  (let* ((chat (cursor-acp--session-chat-buffer sess))
         (input (cursor-acp--session-input-buffer sess))
         (wins (cursor-acp--ensure-pane sess force-width))
         (chat-win (car wins)))
    (when (window-live-p chat-win)
      (set-window-buffer chat-win chat))
    (with-current-buffer chat
      (cursor-acp-chat-mode)
      (setq-local header-line-format (cursor-acp--chat-header-line sess)))
    (with-current-buffer input
      (cursor-acp-input-mode))
    (cursor-acp--input-focus sess)))

(provide 'cursor-acp-ui)
;;; cursor-acp-ui.el ends here
