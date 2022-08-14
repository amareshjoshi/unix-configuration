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
                                        ; asciidoc
                      adoc-mode
                                        ; completion
                      company
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
                                        ; flycheck
                      flycheck
                      flycheck-aspell
                      flycheck-guile
                                        ; graphql utilities
                      graphql
                                        ; graphql documentation explorer
                      graphql-doc
                      graphql-mode
                                        ; launch external programs
                      launch
                                        ; git
                      magit
                                        ; markdown
                      markdown-mode
                                        ;
                      pkg-info
                                        ; project management
                      projectile
                                        ; matching colored delemiters
                      rainbow-delimiters
                                        ; smartparens replaces paredit
                      smartparens
                                        ; package configurator
                      use-package
                                        ; web-mode replaces php-mode
                      web-mode
                                        ; show options after a prefix key
                      which-key
                                        ; yaml
                      yaml-mode))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))
