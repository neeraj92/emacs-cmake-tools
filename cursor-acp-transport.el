;;; cursor-acp-transport.el --- Cursor ACP transport -*- lexical-binding: t; -*-

(require 'cursor-acp-core)
(require 'cursor-acp-ui)

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

(defun cursor-acp--rpc-notify (sess method params)
  (let* ((msg `((jsonrpc . "2.0") (method . ,method) (params . ,params)))
         (json (cursor-acp--json-encode msg))
         (line (concat json "\n"))
         (proc (cursor-acp--session-process sess)))
    (cursor-acp--log sess "-->" json)
    (process-send-string proc line)))

(defun cursor-acp--rpc-respond (sess id result)
  (let* ((msg `((jsonrpc . "2.0") (id . ,id) (result . ,result)))
         (json (cursor-acp--json-encode msg))
         (line (concat json "\n"))
         (proc (cursor-acp--session-process sess)))
    (cursor-acp--log sess "-->" json)
    (process-send-string proc line)))

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

(defun cursor-acp--terminal-stop-reason-p (sr)
  (and (stringp sr)
       (member sr '("end_turn" "cancelled" "canceled" "cancelled_by_user" "user_cancelled"))))

(defun cursor-acp--handle-permission-request (sess id params)
  (setf (cursor-acp--session-permission-request-id sess) id)
  (setf (cursor-acp--session-permission-request-params sess) params)
  (cursor-acp--set-awaiting-permission sess t)
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
     ((and id method (string-equal method "session/request_permission"))
      (cursor-acp--handle-permission-request sess id (gethash "params" msg nil)))
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
        (when (cursor-acp--terminal-stop-reason-p sr)
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
          (progn
            (when (and (hash-table-p update)
                       (string-equal (cursor-acp--ht-get update "sessionUpdate") "tool_call_update")
                       (cursor-acp--ht-get update "rawOutput"))
              (cursor-acp--render-tool-call-raw-output
               sess
               (cursor-acp--ht-get update "toolCallId")
               (cursor-acp--ht-get update "rawOutput")))
            (cursor-acp--render-info sess))))))))

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

(defun cursor-acp--ensure-connected-session (sess)
  (unless (and (cursor-acp--session-process sess)
               (process-live-p (cursor-acp--session-process sess))
               (cursor-acp--session-session-id sess))
    (cursor-acp-start))
  (cursor-acp--session-session-id sess))

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

(provide 'cursor-acp-transport)
;;; cursor-acp-transport.el ends here
