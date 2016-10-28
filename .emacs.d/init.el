;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;
;; portable ~/.emacs.d/init.el file for Amaresh Joshi
;;
;; should work across platforms (linux and mac)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; platform specific stuff (linux, apple, ...)
(when (eq system-type 'darwin) ;; mac specific settings
  ;;
  ;; set PATH and exec-path
  (setq path (concat
                  "/Users/joshia/bin:/Library/TeX/texbin:"
                  "/opt/local/bin:/opt/local/sbin:"
                  "/bin:/usr/bin:"
                  "/sbin:/usr/sbin"))
  (setenv "PATH" path)
  ;;
  ;; exec-path is a list of directories
  (setq exec-path (append (split-string path ":"))) 
  ;;(setq exec-path (append '("/foo/bar/bin")))
  ;;
  ;; default font (for now)
  ;;(set-default-font "Monaco-14")
  ;;(set-default-font "Source Code Pro-14" t t)
  ;; key bindings
  ;;
  ;; values can be:
  ;; 'control, 'alt, 'meta, 'super, 'hyper, nil 
  ;; (setting to nil allows the OS to assign values)
  ;; not sure what alt does
  (setq mac-function-modifier nil)
  (setq mac-control-modifier  'control)
  (setq mac-option-modifier   'meta)
  (setq mac-command-modifier  'super)
  (setq mac-right-control-modifier 'control)
  (setq mac-right-option-modifier  'meta)
  (setq mac-right-command-modifier 'super)
  ;;
  ;; dired settings
  ;; mac "ls" doesn't grok the --dired option
  (setq dired-use-ls-dired nil)
  ;;
  ;; for codeacademy courses that use python2
  (setq python-shell-interpreter "python2.7")
  )

(when (eq system-type 'gnu/linux) ;; linux specific settings
  ;; gnu/linux stuff
  ;;(set-default-font "Source Code Pro-11" t t)
  ;;
  ;; dired settings
  ;; linux "ls" uses the --dired option
  (setq dired-use-ls-dired t)
  )

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
;;(defalias 'yes-or-no-p 'y-or-n-p)

;;
;; rainbow delimiters (someday)
;;
;; (add-hook 'foo-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


;;
;; improves display performance, maybe ...
;; https://masteringemacs.org/article/improving-performance-emacs-display-engine
(setq redisplay-dont-pause t)

;;
;; build and test RE's on the fly
;; https://masteringemacs.org/article/re-builder-interactive-regexp-builder
;;(require 're-builder)
;;(setq reb-re-syntax 'string)

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
;; clean and quiet startup
(setq inhibit-startup-screen t)
(setq initial-scratch-message "the way is void...")
(setq visible-bell nil)

;;
;; fix cut and paste in X
;; see http://emacswiki.org/emacs/CopyAndPaste
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)

;;
;; colors/themes
;;
;; themes only work with emacs24 or higher
;; older emacs need to load the theme package manually
;; so just skip it for now.
(when (>= emacs-major-version 24) 
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (load-theme 'zenburn t)
)

;;
;; always split windows horizontally
(setq split-height-threshold 80)
(setq split-width-threshold nil)

;;
;; sigh ...
(setq gnus-default-nntp-server "news.eternal-september.org")

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
;; and line highlighting
(setq line-number-mode t)
(setq column-number-mode t)
(global-hl-line-mode t)

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
;; eshell and (r(ecursive))grep
(global-set-key (kbd "<f2>" ) 'other-window) ;; avoid using f1
(global-set-key (kbd "<f3>" ) 'eshell)
(global-set-key (kbd "<f4>" ) 'grep)
(global-set-key (kbd "<f5>" ) 'rgrep)
;; windows key is super
(global-set-key (kbd "<s-f12>" ) 'a2ps-buffer)
;;
;; avoid printing by mistake in osX with command-p 
;;default: (global-set-key (kbd "S-p" ) 'ns-print-buffer)
;;(global-unset-key (kbd "S-p" ))
;;(global-set-key (kbd "S-p" ) 'shell)
;;(global-unset-key "\S-p")

;;
;; can also define a particular vkey map as follows:
;(define-key global-map "\M-." 'set-mark-command)
;(define-key text-mode-map "\M-r" 'replace-regexp)
;;
;; can use <fN> to define fuction keys

;;
;; set indentation for perl-mode
(setq perl-indent-level 4)
(setq perl-continued-statement-offset 0)
(setq perl-continued-brace-offset 4)
(setq perl-brace-offset -4)
(setq perl-brace-imaginary-offset 0)
(setq perl-label-offset -4)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eshell stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-face (str &rest properties)
    `(propertize ,str 'face (list ,@properties)))

(defun arj-eshell-prompt ()
  (let ((header-bg "#333")
        )
    (concat
     ;;(with-face (format-time-string "(%Y-%m-%d %H:%M) " (current-time)) :background header-bg :foreground "#888")
     (with-face
      (or (ignore-errors (format "(%s)" (vc-responsible-backend default-directory))) "")
      :background header-bg)
     ;;(with-face user-login-name :foreground "blue" :background header-bg)
     user-login-name
     "@"
     ;;(with-face (concat "" (car (split-string (shell-command-to-string "hostname") "[.\n]"))) :foreground "green")
     (car (split-string (shell-command-to-string "hostname") "[.\n]"))
     ": "
     ;;(with-face (concat (eshell/pwd) " ") :background header-bg)
     (with-face (concat (eshell/pwd) " ") :foreground "green")
     (if (= (user-uid) 0)
         (with-face " #" :foreground "red")
       " $")
     " ")))
(setq eshell-prompt-function 'arj-eshell-prompt)
(setq eshell-highlight-prompt nil)
;;(setq eshell-highlight-prompt 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eshell stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;
;; ;;
;; ;; markdown
;; ;;
;; (autoload 'markdown-mode "markdown-mode"
;;   "Major mode for editing Markdown files" t)
;; (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; unused or obsolete stuff.......
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






