;;
;; for more info on (use-package) see: https://github.com/jwiegley/use-package


;; latex NOT auctex
;;--(use-package latex)
;;--(use-package cider)

;; ;;--(use-package clojure-mode)
;;--(use-package dash)
;;--(use-package epl)
;;--(use-package fill-column-indicator)

;;
;; graphical fill column indicator
;; toggle with ``fci-mode''
(require 'fill-column-indicator)

;;--(use-package flycheck)
;;--(use-package geiser )
;; ;;--(use-package inf-ruby)
;;--(use-package magit)
;;--(use-package markdown-mode)
;;--(use-package markdown-mode+)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; markdown
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
                                        ; Enable Flyspell mode for Karkdown
(add-hook 'markdown-mode-hook 'flyspell-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; smartparens replaces paredit
;; ;;--(use-package paredit)
;; ;;--(use-package php-mode)
;; ;;--(use-package perl6-mode)
;;--(use-package pkg-info)
;;--(use-package projectile)

;;--(use-package quack)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; scheme: racket/guile/chicken
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(require 'quack)
;;
;; name files *.scm
;; stick this in as needed:
;; -*- geiser-scheme-implementation: guile -*-
;; -*- geiser-scheme-implementation: racket -*-
;; -*- geiser-scheme-implementation: chicken -*-
;;
;; probably don't need this
;; quack
(setq quack-default-program scheme-program-name)
;; geiser
(setq geiser-racket-binary scheme-program-name)
;;
;; how to start quack only with scheme files?
;; these don't work:
;; (add-hook 'scheme-mode-hook (require 'quack) t)
;; (add-hook 'scheme-mode-hook #'quack-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;--(use-package queue)
;;--(use-package rainbow-delimiters)
;;--(use-package robe)
;; ;;--(use-package scala-mode)
;;--(use-package seq)
;; smartparens replaces paredit
;;--(use-package smartparens)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; smartparens (replaces paredit)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(smartparens-global-mode 1)
;;
;; not needed for smart parens
;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;--(use-package spinner)


;;;;;;;;;;;;
