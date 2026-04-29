;;; cursor-acp-commands.el --- Cursor ACP commands -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cursor-acp-core)
(require 'cursor-acp-ui)
(require 'cursor-acp-transport)

(defun cursor-acp-focus-input ()
  (interactive)
  (let* ((buf (current-buffer))
         (sess (or (cursor-acp--session-for-buffer buf)
                   (cursor-acp--active-session)
                   (cursor-acp--ensure-session))))
    (cursor-acp--input-focus sess)))

(defun cursor-acp-send ()
  (interactive)
  (let* ((buf (current-buffer))
         (sess (or (cursor-acp--session-for-buffer buf)
                   (cursor-acp--active-session)
                   (cursor-acp--ensure-session)))
         (text (if (eq buf (cursor-acp--session-input-buffer sess))
                   (string-trim-right (buffer-substring-no-properties (point-min) (point-max)))
                 (cursor-acp--input-text sess))))
    (when (string-empty-p (string-trim text))
      (user-error "Input is empty"))
    (if (eq buf (cursor-acp--session-input-buffer sess))
        (let ((inhibit-read-only t)) (erase-buffer))
      (cursor-acp--input-clear sess))
    (cursor-acp--chat-append-user sess text)
    (cursor-acp--chat-open-assistant sess)
    (cursor-acp--set-busy sess t)
    (let ((sid (cursor-acp--ensure-connected-session sess)))
      (cursor-acp--set-active-session-id sid)
      (cursor-acp--rpc-send-async
       sess "session/prompt"
       `((sessionId . ,sid)
         (prompt . [((type . "text") (text . ,text))]))
       sid))
    (cursor-acp--input-focus sess)))

(defun cursor-acp-cancel-turn ()
  "Request cancellation of the current ACP turn."
  (interactive)
  (let* ((sess (or (cursor-acp--session-for-buffer (current-buffer))
                   (cursor-acp--active-session)
                   (cursor-acp--ensure-session)))
         (sid (cursor-acp--ensure-connected-session sess)))
    (unless (cursor-acp--session-busy sess)
      (user-error "No active turn to cancel"))
    (cursor-acp--cancel-pending-permission sess)
    (cursor-acp--rpc-notify sess "session/cancel" `((sessionId . ,sid)))
    (cursor-acp--chat-insert sess "\n[cancel requested]\n" t)
    (message "Cancel requested; waiting for agent confirmation")))

(defun cursor-acp-reprompt-permission ()
  "Re-open prompt for a pending permission request."
  (interactive)
  (cursor-acp--reprompt-pending-permission
   (or (cursor-acp--session-for-buffer (current-buffer))
       (cursor-acp--active-session)
       (cursor-acp--ensure-session))))

(defun cursor-acp-start ()
  "Start ACP and create a session."
  (interactive)
  (let ((sess (cursor-acp--ensure-session)))
    (cursor-acp--ensure-process sess)
    (condition-case err
        (progn
          (cursor-acp--handshake sess)
          (let ((s (or (cursor-acp--active-session) sess)))
            (with-current-buffer (cursor-acp--session-chat-buffer s)
              (setq-local header-line-format (cursor-acp--status-line s)))
            (cursor-acp--render-info s)))
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
    (when-let ((proc (cursor-acp--conn-process (cursor-acp--ensure-conn))))
      (ignore-errors (delete-process proc)))
    (setf (cursor-acp--conn-process (cursor-acp--ensure-conn)) nil)
    (cursor-acp--log sess "process" "killed by user")
    (with-current-buffer (cursor-acp--session-chat-buffer sess)
      (setq-local header-line-format (cursor-acp--status-line sess)))
    (cursor-acp--render-info sess)))

(defun cursor-acp-show-info ()
  (interactive)
  (let ((sess (cursor-acp--ensure-session)))
    (cursor-acp--render-info sess)
    (pop-to-buffer (cursor-acp--session-info-buffer sess))
    (cursor-acp-info-mode)))

(defun cursor-acp-show-logs ()
  "Show the ACP log buffer."
  (interactive)
  (pop-to-buffer (cursor-acp--session-log-buffer (cursor-acp--ensure-session))))

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

(defun cursor-acp-switch-session ()
  "List sessions and switch to one (load/resume), then reset layout."
  (interactive)
  (let ((bootstrap (cursor-acp--ensure-session)))
    (cursor-acp--ensure-process bootstrap)
    (when (or (not (process-live-p (cursor-acp--conn-process (cursor-acp--ensure-conn))))
              (not (cursor-acp--conn-init-result (cursor-acp--ensure-conn))))
      (cursor-acp-start))
    (let* ((sess (cursor-acp--ensure-session))
           (items
            (if (cursor-acp--cap-session-list-p)
                (let* ((res (cursor-acp--rpc-session-list sess))
                       (sessions (and (hash-table-p res) (cursor-acp--ht-get res "sessions"))))
                  (cl-remove-if-not #'hash-table-p (cursor-acp--normalize-items sessions)))
              (let (acc)
                (when (hash-table-p cursor-acp--sessions)
                  (maphash
                   (lambda (k v)
                     (ignore k)
                     (when (and (cursor-acp--valid-session-p v)
                                (stringp (cursor-acp--session-session-id v)))
                       (let ((ht (make-hash-table :test 'equal)))
                         (puthash "sessionId" (cursor-acp--session-session-id v) ht)
                         (puthash "title" (cursor-acp--session-title v) ht)
                         (push ht acc))))
                   cursor-acp--sessions))
                (nreverse acc))))
           (alist
            (mapcar
             (lambda (it)
               (let* ((sid (cursor-acp--ht-get it "sessionId"))
                      (title (or (cursor-acp--ht-get it "title") "Untitled"))
                      (cwd (cursor-acp--ht-get it "cwd"))
                      (updated (or (cursor-acp--ht-get it "updatedAt") "")))
                 (cons (format "%s%s"
                               title
                               (if (string-empty-p (string-trim updated)) "" (format "  (%s)" updated)))
                       (list sid title cwd))))
             items))
           (choice (completing-read "Session: " (mapcar #'car alist) nil t))
           (sel (cdr (assoc choice alist)))
           (sid (nth 0 sel))
           (title (nth 1 sel))
           (cwd (nth 2 sel)))
      (unless (stringp sid)
        (user-error "No session selected"))
      (let ((target (cursor-acp--ensure-session-by-id sid title cwd)))
        (cursor-acp--set-active-session-id sid)
        (cursor-acp--ensure-session-buffers target)
        (cursor-acp-reset-layout)
        (cursor-acp--chat-clear target)
        (if (cursor-acp--cap-load-session-p)
            (cursor-acp--rpc-session-load target sid)
          (when (cursor-acp--cap-session-resume-p)
            (cursor-acp--rpc-session-resume target sid)))))))

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

(defun cursor-acp ()
  "Open the Cursor ACP buffer."
  (interactive)
  (let ((sess (cursor-acp--ensure-session)))
    (cursor-acp--ensure-connected-session sess)
    (cursor-acp--open-ui (or (cursor-acp--active-session) sess))))

(provide 'cursor-acp-commands)
;;; cursor-acp-commands.el ends here
