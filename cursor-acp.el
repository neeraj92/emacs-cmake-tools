;;; cursor-acp.el --- Cursor ACP client loader -*- lexical-binding: t; -*-

(let ((load-prefer-newer t))
  (require 'cursor-acp-core)
  (require 'cursor-acp-ui)
  (require 'cursor-acp-transport)
  (require 'cursor-acp-commands))

(provide 'cursor-acp)
;;; cursor-acp.el ends here
