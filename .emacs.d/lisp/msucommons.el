;;
;; humcore utility functions

(defun visit-humcore ()
  "Go to the humcore folder on the Commons development server."
  (interactive)
  (progn
    ;;
    ;; to open up bash instead of zsh on the remote server
    (setenv "SHELL" "/bin/bash")
    (setenv "ESHELL" "/bin/bash")
    (find-file "/ssh:ubuntu@msudev:/srv/www/commons/current/web/app/plugins/humcore/.")))

