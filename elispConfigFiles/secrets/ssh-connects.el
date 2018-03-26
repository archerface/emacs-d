;;; ssh-connects.el --- Private ssh connections for work.

;;; Commentary:
;; This file contains interactive functions for connecting to work servers with TRAMP.
;; This file will not show up in a git repository due to its sensitive nature.


;;; Code:
(defun flower-client-connect ()
  "This function connects to the work ssh server for the client code, interactively!"
  (interactive)
  (dired "/ssh:purpledev@dev.fiftyflowers.com:/ebs/sites/flower0/dev9"))

(defun flower-admin-connect ()
  "Function to dive into the great depths of hell itself, legacy php system dev admin."
  (interactive)
  (dired "/ssh:purpledev@dev.fiftyflowers.com:/ebs/sites/ssl/flower0/admin_dev9"))

(provide 'ssh-connects)
;;; ssh-connects.el ends here
