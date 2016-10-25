;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;
;; linuxvm-emacs23.el
;;
;; .emacs file for CAL/CeLTA/CLEAR linux VMs
;;
;; for now (2016.09) these VMs use emacs version 23
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; dired settings
;; linux "ls" uses the --dired option
(setq dired-use-ls-dired t)

;;
;; add my own elisp directory to the loadpath
(add-to-list 'load-path "~/.emacs.d/lisp")

;;
;; utf-8 encoding
(set-language-environment "UTF-8") ;; also bound to: C-x RET l
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;;

;;
;; terminal vs windows specific stuff 
(cond ((display-graphic-p)
       (menu-bar-mode t)
       ;;
       ;; get rid of scroll and tool bar
       (tool-bar-mode -1)
       (scroll-bar-mode -1)
       )
      ;;
      ;; terminal stuff
      (t 
       (menu-bar-mode -1)
       )
)

;;
;; answer just y/n to to yes/no question prompts
;;
(defalias 'yes-or-no-p 'y-or-n-p)

;;
;; improves display performance, maybe ...
;; https://masteringemacs.org/article/improving-performance-emacs-display-engine
(setq redisplay-dont-pause t)

;;
;; build and test RE's on the fly
;; https://masteringemacs.org/article/re-builder-interactive-regexp-builder
(require 're-builder)
(setq reb-re-syntax 'string)

;;
;; font coloring
(global-font-lock-mode t)
;;
;; and more coloring
(setq font-lock-global-modes t)
(setq font-lock-maximum-decoration t)

;; also highlight parens
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;;
;;
(setq inhibit-startup-screen t)

;;
;; always split windows horizontally
(setq split-height-threshold 80)
(setq split-width-threshold nil)

;;
;; force emacs to insert tabs instead of spaces
(setq-default indent-tabs-mode nil)


;;
;; set the title bar to show file name if available, buffer name otherwise
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;;
;; buffers with these names will open sepearte windows (frames)
;; under X
(setq special-display-buffer-names
      '("*Colors*" "*Faces*"))

;;
;;  make no backups
(setq backup-inhibited t)
(setq version-control ())
(setq make-backup-files ())
;;
;; don't make #backups# if choose not to save a file
;; also don't make 'dead' pointers if a session quits suddenly
;;
;; (doesn't work!)
(setq auto-save-list-file-prefix nil) 

;;
;; enable the {up|down}case region commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;
;; display time and date on modeline
(setq display-time-day-and-date t)
(display-time)

;;
;; turn on line and column number modes
(setq line-number-mode t)
(setq column-number-mode t)

;;
;; set global keymaps
;;
;; toggle global line highlight mode (highlights the current line)
(global-set-key "\M-h" 'global-hl-line-mode)
(global-set-key "\M-." 'set-mark-command)
;
;;(global-set-key "\M-r" 'replace-regexp)
;;(global-set-key "\M-s" 'replace-string)
;;;                                     
(global-set-key (kbd "M-r" ) 'replace-regexp)
(global-set-key (kbd "M-s" ) 'replace-string)
;;
;; remap other-window
(global-set-key (kbd "M-o" ) 'other-window)
;;
;; shell and (r(ecursive))grep
(global-set-key (kbd "<f2>" ) 'shell) ;; avoid using f1
(global-set-key (kbd "<f3>" ) 'grep)
(global-set-key (kbd "<f4>" ) 'rgrep)
;; windows key is super
(global-set-key (kbd "<s-f12>" ) 'a2ps-buffer)

;;
;;   set the indentation for C-mode
(setq c-indent-level 4)
(setq c-continued-statement-offset 4)
(setq c-brace-offset -4)
(setq c-argdecl-indent 8)

;; set indentation for python
(setq python-indent-offset 4)
;;

;;
;;     the string called when compile is called
(setq compile-command "make")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; language specific packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; markdown
;;
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; unused or obsolete stuff.......
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
