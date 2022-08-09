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
                      ;;-----------------------------------
                      aggressive-indent
                      adoc-mode         ; ascii doc
                      ;; clojure --------------------------
                      clojure-mode
                      clojure-mode-extra-font-locking
                      cider
                      ;;-----------------------------------
                      dash
                      epl
                      flycheck
                      ;; scheme stuff
                      geiser
                      geiser-guile
                      ;;-----------------------------------
                      graphql ;; graphql utilities
                      graphql-doc ;; graphql documentation explorer
                      graphql-mode
                      launch
                      magit
                      markdown-mode
                      pkg-info
                      projectile
                      rainbow-delimiters
                      smartparens ;; smartparens replaces paredit
                      use-package
                      ;; web-mode replaces php-mode
                      web-mode
                      which-key
                      yaml-mode))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))
