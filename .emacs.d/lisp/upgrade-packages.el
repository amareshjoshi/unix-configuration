;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ~/.emacs.d/lisp/load-packages.el
;;
;; ensures installed packages are the same on all platforms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq url-http-attempt-keepalives nil)

(defvar my-packages '(
                                        ; LaTeX ide
                      auctex
                      cdlatex
                      adoc-mode
                                        ; common lisp ide
                      slime
                                        ; clojure ide
                      clojure-mode
                      clojure-mode-extra-font-locking
                      cider
                                        ; scheme ide
                      geiser-guile
                      geiser-chez
                      quack
                                        ; ---
                      dash
                      epl
                      flycheck
                                        ; graphql utilities
                      graphql
                                        ;graphql documentation explorer
                      graphql-doc
                      graphql-mode
                      launch
                      magit
                      markdown-mode
                      pkg-info
                      projectile
                      rainbow-delimiters
                                        ; smartparens replaces paredit
                      smartparens 
                      use-package
                                        ; web-mode replaces php-mode
                      web-mode
                      which-key
                      yaml-mode))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))
