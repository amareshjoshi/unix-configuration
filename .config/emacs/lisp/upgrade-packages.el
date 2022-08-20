;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ~/.emacs.d/lisp/upgrade-packages.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq url-http-attempt-keepalives nil)

(defvar my-packages '(
                                        ; lsp
                      lsp-mode
                      typescript-mode
					; javascript mode (maintained by Steve Yegge)
                      js2-mode
                                        ; LaTeX
                      auctex
                      cdlatex
                                        ; asciidoc
                      adoc-mode
                                        ; completion inside text buffers
                      company
                                        ; completion ifor emacs commands, file lookup, etc.
                      counsel
                                        ; common lisp ide
                      slime
                                        ; clojure ide
                      clojure-mode
                      clojure-mode-extra-font-locking
                      cider
                                        ; scheme ide
                      geiser-guile
                      geiser-chez
                      geiser-racket
                                        ; flycheck
                      flycheck
                      flycheck-aspell
                      flycheck-guile
		      flycheck-clojure
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
                                        ; web-mode, php-mode
                      web-mode
                      php-mode
                                        ; show options after a prefix key
                      which-key
                                        ; yaml
                      yaml-mode))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))
