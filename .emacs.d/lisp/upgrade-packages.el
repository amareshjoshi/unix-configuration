;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ~/.emacs.d/lisp/load-packages.el
;;
;; ensures installed packages are the same on all platforms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq url-http-attempt-keepalives nil)

(defvar my-packages '(;; TeX stuff
                      auctex
                      cdlatex
                      adoc-mode
                                        ; clojure
                      clojure-mode
                      clojure-mode-extra-font-locking
                      cider
                                        ; clojure
                      dash
                      epl
                      flycheck
                                        ; scheme
                      geiser
                      geiser-guile
                      quack
                                        ; scheme
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
