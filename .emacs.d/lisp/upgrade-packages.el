;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ~/.emacs.d/lisp/load-packages.el
;;
;; ensures installed packages are the same on all platforms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq url-http-attempt-keepalives nil)

(defvar my-packages '(auctex
                      ;; asciidoc
                      aggressive-indent
                      adoc-mode
                      cdlatex
                      ;; clojure --------------------------
                      clojure-mode
                      clojure-mode-extra-font-locking
                      cider
                      ;;inf-clojure
                      ;; clojure ---------------------------
                      dash
                      epl
                      flycheck
                      ;;
                      ;; scheme ide
                      geiser
                      geiser-guile
                      graphql ;; graphql utilities
                      graphql-doc ;; graphql documentation explorer
                      graphql-mode
                      launch
                      magit
                      markdown-mode
                      pkg-info
                      projectile
                      rainbow-delimiters
                      seq
                      smartparens ;; smartparens replaces paredit
                      use-package
                      ;; web-mode replaces php-mode
                      web-mode
                      yaml-mode))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))
