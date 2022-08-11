;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ~/.emacs.d/lisp/load-packages.el
;;
;; for more info on (use-package) see: https://github.com/jwiegley/use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; clojure
;;
;; don't invoke inf-mode and cider mode at the same time
;; cider is like SLIME
;; inf-clojure is simpler and probably better for sicp. but it's freezing
;;
;; https://clojure.org/guides/editors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package clojure-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
  (require 'clojure-mode-extra-font-locking)
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook #'menu-bar-mode))
(use-package cider
  :ensure t
  :pin melpa-stable
  :config
  (menu-bar-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; which-key
;; displays keybindings when you press a prefix key
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :ensure t
  :pin melpa-stable
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; org mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :ensure t
  :config
  (define-key org-mode-map (kbd "C-c l") 'org-store-link)
  (define-key org-mode-map (kbd "C-c a") 'org-agenda)
  (setq org-log-done t)
  ;; enable flyspell mode for org
  (add-hook 'org-mode-hook 'flyspell-mode)
  ;; set the list of packages that will be used by LaTeX export
  (setq org-latex-packages-alist '())
                                        ;  tikz ist keine drawing program
  (add-to-list 'org-latex-packages-alist '("" "tikz" t))
                                        ; double and 1.5 line spacing
  (add-to-list 'org-latex-packages-alist '("" "setspace" t)) 
                                        ;  creates dummy text
  (add-to-list 'org-latex-packages-alist '("" "blindtext" t))
                                        ;  math symbols
  (add-to-list 'org-latex-packages-alist '("" "stmaryrd" t))
                                        ;  roof (v3)(was triangle in v2))
                                        ;  see: https://tex.stackexchange.com/questions/291420/tikz-pgf-error-when-using-forest
  (add-to-list 'org-latex-packages-alist '("linguistics" "forest" t))
                                        ;  fancy headers and footers
  (add-to-list 'org-latex-packages-alist '("" "fancyhdr" t))
                                        ;  multiple column text
  (add-to-list 'org-latex-packages-alist '("" "multicol" t))
                                        ;  color (obvs))
  (add-to-list 'org-latex-packages-alist '("" "color" t))
                                        ;  prevents indentation of first line
  (add-to-list 'org-latex-packages-alist '("" "parskip" t))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; build and test RE's on the fly
;; https://masteringemacs.org/article/re-builder-interactive-regexp-builder
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package re-builder
  :defer t
  :config
  (setq reb-re-syntax 'string)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; AUCTeX
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package latex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)                  ; enable parsing on save
  (setq TeX-engine 'xetex)                ; use xetex instead of latex
  (setq TeX-parse-self t)                 ; enable parsing on load
  (setq TeX-save-query nil)               ; If non-nil, then query the user
                                        ; before saving each file with TeX-save-document.
  (setq-default TeX-master nil)           ; set up AUCTeX to deal with
                                        ; multiple file documents.
  (setq LaTeX-biblatex-use-Biber t)       ; use biber by default
  (setq TeX-PDF-mode t)                   ; use pdflatex by default
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode) ;turn on pdf-mode.  (how are these different?)
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex) ; lots of kbd shortcuts for latex environments
                                        ; and math mode (see CDLatex documentation)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (TeX-fold-mode 1)))         ;turn on tex-fold-mode by default
  ;; LaTeX-math-mode http://www.gnu.org/s/auctex/manual/auctex/Mathematics.html
  (add-hook 'TeX-mode-hook 'LaTeX-math-mode)
  ;;
  ;; system specific
  ;; mac
  (when (eq system-type 'darwin)
    (setq TeX-output-view-style             ; default viewers for AUCTeX
          '(("^pdf$" "." "/Applications/Preview.app/Contents/MacOS/Preview")
            )))
  ;; linux
  (when (eq system-type 'gnu/linux)
    (setq TeX-output-view-style             ; default viewers for AUCTeX
          '(;;
            ;; this will need to be tweaked to work with both Linux and WSL
            ;;
            ;; WSL
            ;; may have to do something similar as org mode (see above)
            ;; ("^pdf$" "." "/mnt/c/Program\\ Files\\ \\(x86\\)/Adobe/Acrobat\\ 11\\.0/Acrobat/Acrobat.exe %o")
            ;;
            ;; regular Linux
            ("^pdf$" "." "evince -f %o")
            ("^html?$" "." "iceweasel %o")))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RefTeX
;;
;; see: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package reftex
  :ensure t
  :config
  ;; http://www.gnu.org/s/auctex/manual/reftex/reftex_5.html
  (add-hook 'TeX-mode-hook 'turn-on-reftex)
  ;; enable flyspell mode for TeX modes such as AUCTeX
  (add-hook 'TeX-mode-hook 'flyspell-mode)
  ;; make reftex and auctex play nice together
  (setq reftex-plug-into-AUCTeX t))


;;--(use-package dash)
;;--(use-package epl)
;;--(use-package fill-column-indicator)

;;
;; graphical fill column indicator
;; toggle with ``fci-mode''
;;(require 'fill-column-indicator)

;;--(use-package flycheck)
;;--(use-package geiser )
;; ;;--(use-package inf-ruby)
;;--(use-package magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; launch
;;
;; Launch files and directories using the associated applications provided by
;; your operating system.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package launch
  :ensure t
  :config
  (global-launch-mode +1)
  ;; If you only want to enable it for certain modes, add:
  (add-hook 'dired-mode 'turn-on-launch-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; markdown-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :ensure t
  :config
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
                                        ; Enable Flyspell mode for markdown
  (add-hook 'markdown-mode-hook 'flyspell-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; web-mode (html, javascript, css, php)
;; (replaces php-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(require 'web-mode)
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  ;; php "inc"lude files
  (add-to-list 'auto-mode-alist '("\\.inc\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
  ;; enable flyspell mode
  (add-hook 'web-mode-hook 'flyspell-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; smartparens (replaces paredit)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smartparens
  :ensure t
  :config
  ;; good defaults
  (require 'smartparens-config)
  ;; turn on globally
  (smartparens-global-mode 1)
  ;; maybe *too* strict 
  (smartparens-strict-mode 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; rainbow delimiters
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; projectile (project management)
;;
;; This library provides easy project management and navigation.  The
;; concept of a project is pretty basic - just a folder containing
;; special file.  Currently git, mercurial and bazaar repos are
;; considered projects by default.  If you want to mark a folder
;; manually as a project just create an empty .projectile file in
;; it.  See the README for more details.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile
  :ensure t
)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; scheme: just guile (for now)
;;
;; install: quack, geiser, geiser-guile
;;
;; name files *.scm
;; stick this in as needed:
;; -*- geiser-scheme-implementation: guile -*-
;; -*- geiser-scheme-implementation: racket -*-
;; -*- geiser-scheme-implementation: chicken -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package quack
  :ensure t
  :config
  (when (eq system-type 'darwin) ;; mac specific settings
    (setq scheme-program-name "/usr/local/bin/guile")
    )
  (when (eq system-type 'gnu/linux) ;; linux specific settings
    (setq scheme-program-name "/usr/bin/guile3.0")
    )
  (setq geiser-guile-binary scheme-program-name)
  )
(use-package geiser
  :ensure t
  :config
  (setq the-way '(is void))
  )
(use-package geiser-guile
  :ensure t
  :config
  (setq the-way '(is void))
  )

;;
;; eof
;;
