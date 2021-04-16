;;
;; ensures installed packages are the same on all platforms
;; run using
;; M-x eval-buffer

;;
;; if you get gpg errors this is because your version of emacs doesn't
;; have the latest gpg keys to fix:
;; 1. (setq package-check-signature nil)
;; 2. update package list and install the package: gnu-elpa-keyring-update-*
;; 3. reset the old value back: (setq package-check-signature 'allow-unsigned)
;;(setq package-check-signature 'nil)
(setq package-check-signature 'allow-unsigned)
;;
;; package setup
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://melpa.org/packages/")
                         ("orgmode" . "http://orgmode.org/elpa/")))
(package-initialize)
(setq url-http-attempt-keepalives nil)

(defvar my-packages '(auctex
                      ;; asciidoc
                      adoc-mode
                      cdlatex
                      ;;cider
                      ;;clojure-mode
                      dash
                      epl
                      flycheck
                      ;;geiser 
                      launch
                      magit
                      markdown-mode
                      markdown-mode+
                      pkg-info
                      projectile
                      ;;quack
                      queue
                      rainbow-delimiters
                      seq
                      ;; smartparens replaces paredit
                      smartparens
                      spinner
                      use-package
                      ;; web-mode replaces php-mode
                      web-mode))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))
