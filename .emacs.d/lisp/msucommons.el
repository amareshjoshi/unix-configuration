;;
;; humcore utility functions

;;
;; should really combine these into a single "general" command

(defun visit-humcore-dev ()
  "Go to the humcore folder on the Commons development server."
  (interactive)
  (progn
    ;;
    ;; to open up bash instead of zsh on the remote server
    (setenv "SHELL" "/bin/bash")
    (setenv "ESHELL" "/bin/bash")
    (find-file "/ssh:ubuntu@msudev:/srv/www/commons/current/web/app/plugins/humcore/.")))

(defalias 'visit-humcore 'visit-humcore-dev)

(defun visit-humcore-prod ()
  "Go to the humcore folder on the Commons development server."
  (interactive)
  (progn
    ;;
    ;; to open up bash instead of zsh on the remote server
    (setenv "SHELL" "/bin/bash")
    (setenv "ESHELL" "/bin/bash")
    (find-file "/ssh:ubuntu@hcommons:/srv/www/commons/current/web/app/plugins/humcore/.")))

