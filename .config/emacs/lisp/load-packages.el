;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; load packages
;;
;; ~/.emacs.d/lisp/load-packages.el
;;
;; for more info on (use-package) see: https://github.com/jwiegley/use-package
;;
;; nice tutorial:
;; https://ianyepan.github.io/posts/setting-up-use-package/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lsp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l") ;; or 'C-l' or  's-l'
  :config
  (lsp-enable-which-key-integration t))
;;
;; TS
(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))
;;
;; JS
(use-package js2-mode
  :ensure t
  :hook (js2-mode . lsp-deferred)
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scala
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lua
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lua-mode
  :interpreter
  ("lua" . lua-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  ;;
  ;; need to define some lua mode specific keybindings
  ;; e.g. lua-send-current-line
  ;; the only one that comes builtin is send-buffer (C-c C-l)
  (define-key lua-mode-map (kbd "C-c C-e") 'lua-send-current-line)
  (define-key lua-mode-map (kbd "C-c C-r") 'lua-send-region)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :pin melpa)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; company (completion inside buffers (text))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :pin melpa
  :hook 
  ;; turn on everywhere
  (after-init-hook . global-company-mode)
  ;; (scheme-mode-hook . company-mode)
  :config
  ;; No delay in showing suggestions.
  (setq company-idle-delay 0)
  ;; Show suggestions after entering one character.
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  ;; Use tab key to cycle through suggestions.
  ;; ('tng' means 'tab and go')
  (company-tng-configure-default)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vertico/consult
;; replaces consel/ivy/swiper
;;
;; savehist - save completion history
;; orderless - completion style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  ;; save completion history
  (savehist-mode))
;;
;; Use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; counsel, ivy, and swiper (completion for emacs commands)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package counsel
;;   :ensure t
;;   :pin melpa
;;   :config
;;   (ivy-mode 1)
;;   ;; When non-nil, add recent files and/or bookmarks to ‘ivy-switch-buffer’.
;;   ;; The value ‘recentf’ includes only recent files to the virtual
;;   ;; buffers list, whereas the value ‘bookmarks’ does the same for
;;   ;; bookmarks.  Any other non-nil value includes both.

;;   (setq ivy-use-virtual-buffers nil)
;;   (setq ivy-count-format "(%d/%d) "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck check for errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  :ensure t
  :pin melpa
  :init
  (global-flycheck-mode t)
  :config
  (require 'flycheck-aspell)
  (require 'flycheck-guile)
  (require 'flycheck-clojure)
  )

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
  :pin melpa
  :hook
  (clojure-mode-hook . cider-mode)
  (clojure-mode-hook . subword-mode)
  (clojure-mode-hook . smartparens-strict-mode)
  (clojure-mode-hook . rainbow-delimiters-mode)
  (clojure-mode-hook . aggressive-indent-mode)
  (clojure-mode-hook . menu-bar-mode))

  :config
  (add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
  (require 'clojure-mode-extra-font-locking)
(use-package cider
  :ensure t
  :pin melpa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; which-key
;; displays keybindings when you press a prefix key
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :ensure t
  :pin melpa
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; org mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :ensure t
  :pin melpa
  :hook
  (org-mode-hook . flyspell-mode)
  :config
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)
  ;; we use M-<arrow> to move around windows
  ;; so redefine to C-M-<arrow>
  (define-key org-mode-map (kbd "<C-M-up>") 'org-metaup)
  (define-key org-mode-map (kbd "<C-M-down>") 'org-metadown)
  (define-key org-mode-map (kbd "<C-M-left>") 'org-metaleft)
  (define-key org-mode-map (kbd "<C-M-right>") 'org-metaright)
  (setq org-latex-pdf-process
	(list "latexmk -pdf %f"))
  ;;
  ;; default export settings (these can be overridden in individual files by setting option to "t")
					; no TOC (#+OPTIONS: toc:t)
  (setq org-export-with-toc nil)
					; disable TeX sub/super scripting (#+OPTIONS: ^:t)
  (setq org-export-with-sub-superscripts nil)
					; turn off automatic section numbering (#+OPTIONS: num:t)
  (setq org-export-with-section-numbers nil)
  (setq org-log-done t)
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
					; fancy quotes
  (add-to-list 'org-latex-packages-alist '("strict=true,english=american,style=american" "csquotes" t))
					; fancy quotes
  (add-to-list 'org-latex-packages-alist '("strict=true, english=american, style=american" "csquotes" t))
					; glosses and numbered examples
					; needs to be last or near the end
  (add-to-list 'org-latex-packages-alist '("" "gb4e" t))
  (add-to-list 'org-latex-packages-alist
	       '("backend=biber, bibstyle=authoryear, citestyle=authoryear, maxcitenames=3, url=false, maxbibnames=99" "biblatex" t))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; build and test RE's on the fly
;; https://masteringemacs.org/article/re-builder-interactive-regexp-builder
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package re-builder
  :ensure t
  :pin melpa
  :config
  (setq reb-re-syntax 'string)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; AUCTeX
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package latex
;;   :ensure t
;;   :ensure auctex
(use-package auctex
  :ensure t
  :pin melpa
  :hook
  (LaTeX-mode-hook . TeX-PDF-mode)
					; lots of kbd shortcuts for latex math mode (see CDLatex documentation)
  (LaTeX-mode-hook . turn-on-cdlatex)
					;turn on tex-fold-mode by default
  (LaTeX-mode-hook . TeX-fold-mode)
					; LaTeX-math-mode http://www.gnu.org/s/auctex/manual/auctex/Mathematics.html
  (TeX-mode-hook . LaTeX-math-mode)
  
  :config
  (add-to-list 'TeX-command-list
                  '(
					; label
		    "latexMK"
					; shell command
		    "latexmk -pdf %t"
					; lisp function that runs the command
		    TeX-run-shell
					; non interactive
		    nil
					; always active
		    t
					; menu entry
		    :help "Run latexmk")
                  t)
  (setq TeX-command-default "latexMK")
					; enable parsing on save
  (setq TeX-auto-save t)
					; use luatex instead of latex
  (setq TeX-engine 'luatex)
					; use xetex instead of latex
  ;;(setq TeX-engine 'xetex)
					; enable parsing on load
  (setq TeX-parse-self t)
					; If non-nil, then query the user
					; before saving each file with TeX-save-document.
  (setq TeX-save-query nil)

					; set up AUCTeX to deal with
					; multiple file documents.
  (setq-default TeX-master nil)
                                        ; use biber by default
  (setq LaTeX-biblatex-use-Biber t)
					; use pdflatex by default
  (setq TeX-PDF-mode t)
					;turn on pdf-mode.  (how are these different?)
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
  :pin melpa
  :hook
					; http://www.gnu.org/s/auctex/manual/reftex/reftex_5.html
  (TeX-mode-hook . turn-on-reftex)
					; enable flyspell mode for TeX modes such as AUCTeX
  (TeX-mode-hook . flyspell-mode)

  :config
  ;; make reftex and auctex play nice together
  (setq reftex-plug-into-AUCTeX t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; launch
;;
;; Launch files and directories using the associated applications provided by
;; your operating system.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package launch
  :ensure t
  :pin melpa
  :hook
  (dired-mode . turn-on-launch-mode)
  :config
  (global-launch-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; markdown-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :ensure t
  :pin melpa
  :hook
  (markdown-mode-hook . flyspell-mode))
  :config
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
                                        ; Enable Flyspell mode for markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; web-mode (html, javascript, css, php)
;; php-mode needed for php
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package php-mode
  :ensure t
  :pin melpa
  :config
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode)))
(use-package web-mode
  :ensure t
  :pin melpa
  :hook
    (web-mode-hook . flyspell-mode))
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  ;; php "inc"lude files
  (add-to-list 'auto-mode-alist '("\\.inc\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; smartparens (replaces paredit)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smartparens
  :ensure t
  :pin melpa
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
  :pin melpa
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))
  :config
  (+ 1 2)

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
  :pin melpa
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; scheme: just guile (for now)
;;
;; install: quack, geiser, geiser-{guile chez}
;;
;; name files *.scm
;; stick this in as needed:
;; -*- geiser-scheme-implementation: guile -*-
;; -*- geiser-scheme-implementation: kawa -*-
;; -*- geiser-scheme-implementation: chez -*-
;; -*- geiser-scheme-implementation: racket -*-
;; -*- geiser-scheme-implementation: chicken -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; do we need quack??
;; (use-package quack
;;   :ensure t
;;   :pin melpa
;;   :hook
;;   ;; how to turn on for scheme files ONLY?
;;   (scheme-mode-hook . quack-mode-does-not-exist)
;;   :config
;;   (custom-set-variables '(quack-global-menu-p nil))
;;   (custom-set-variables '(quack-default-program "guile")))
(use-package geiser
  :ensure t
  :pin melpa
  :config
  ;;(setq geiser-active-implementations '(guile))
  (setq geiser-active-implementations '(guile chez racket))
  (setq geiser-guile-binary "guile")
  (setq geiser-chez-binary "chez")
  (setq geiser-racket-binary "racket"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SLIME (common-lisp ide)
;;
;; https://github.com/slime/slime
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package slime
  :ensure t
  :pin melpa
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl"))
;;
;; eof
;;
