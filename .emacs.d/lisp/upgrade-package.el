;;
;; ensures installed packages are the same on all platforms
;; run using
;; M-x eval-buffer

;;
;; package setup
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa.org/packages/"))
(package-initialize)
(setq url-http-attempt-keepalives nil)

(defvar my-packages '(auctex
                      cider
                      ;;clojure-mode
                      dash
                      epl
                      fill-column-indicator
                      flycheck
                      geiser 
                      ;;inf-ruby
                      magit
                      markdown-mode
                      markdown-mode+
                      ;;paredit
                      ;;perl6-mode
                      pkg-info
                      projectile
                      quack
                      queue
                      rainbow-delimiters
                      robe
                      ;;scala-mode
                      seq
                      ;; smartparens replaces paredit
                      smartparens
                      spinner
                      ;; web-mode replaces php-mode
                      web-mode))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))
