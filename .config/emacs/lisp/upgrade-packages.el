;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ~/.emacs.d/lisp/upgrade-packages.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq url-http-attempt-keepalives nil)

(defvar my-packages '(
                                        ; package configurator
                      use-package
                                        ; get info on packages
                      pkg-info
                                        ; lsp
                      lsp-mode
                      typescript-mode
					; javascript mode (maintained by Steve Yegge)
                      js2-mode
                                        ; asciidoc
                      adoc-mode
                                        ; markdown
                      markdown-mode
                                        ; graphql utilities
                      graphql
                                        ; graphql documentation explorer
                      graphql-doc
                      graphql-mode
                                        ; web-mode
                      web-mode
					; php-mode (need this in addition to web-mode)
                      php-mode
                                        ; lua
                      lua-mode
                      luarocks
                                        ; scala
                      scala-mode
                                        ; yaml
                      yaml-mode
                                        ; matching colored delemiters
                      rainbow-delimiters
                                        ; smartparens (replaces paredit)
                      smartparens
                                        ; completion inside text buffers
                      company
                                        ; completion for emacs commands, file lookup, etc.
					; replaces counsel/ivy/swiper
					; orderless completion style for vertico
		      vertico
		      orderless
                                        ; show options after a prefix key
                      which-key
                                        ; flycheck
                      flycheck
                      flycheck-aspell
                      flycheck-guile
		      flycheck-clojure
                                        ; LaTeX ide
                      auctex
                      cdlatex
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
                                        ; git ide
                      magit
                                        ; launch external programs
                      launch
                                        ; project management
                      projectile
		      ))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))
