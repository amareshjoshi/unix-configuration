;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;
;; .emacs file for Amaresh Joshi
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
                  "/opt/local/racket/bin:"
                  "/bin:/usr/bin:"
                  "/sbin:/usr/sbin"))
  (setenv "PATH" path)
  ;;
  ;; exec-path is a list of directories
  (setq exec-path (append (split-string path ":"))) 
  ;;(setq exec-path (append '("/foo/bar/bin")))
  ;;
  ;; need this, not sure why
  (setq scheme-program-name "/opt/local/racket/bin/racket")
  ;;
  ;; default font (for now)
  ;;(set-default-font "Monospace-10")
  ;;(set-default-font "Monaco-14")
  (set-default-font "Source Code Pro-14" t t)
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
  (setq TeX-output-view-style             ; default viewers for AUCTeX
        '(("^pdf$" "." "/Applications/Preview.app/Contents/MacOS/Preview")
          ))
  )

(when (eq system-type 'gnu/linux) ;; linux specific settings
  ;; gnu/linux stuff
  (set-default-font "Source Code Pro-11" t t)
  ;;
  ;; dired settings
  ;; linux "ls" uses the --dired option
  (setq dired-use-ls-dired t)
  ;; may not need this
  (setq scheme-program-name "/usr/bin/racket")
  ;;
  (setq TeX-output-view-style             ; default viewers for AUCTeX
        '(("^pdf$" "." "evince -f %o")
          ("^html?$" "." "iceweasel %o")))
  )

;;
;; add my own elisp directory to the loadpath
(add-to-list 'load-path "~/.emacs.d/lisp")

;;
;; package stuff
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa.org/packages/"))
(package-initialize)
(setq url-http-attempt-keepalives nil)

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
;; rainbow delimiters
;;
;; (add-hook 'foo-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


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
;; graphical fill column indicator
;; toggle with ``fci-mode''
(require 'fill-column-indicator)

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
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zenburn t)
;;(load "color-theme-zenburn.el")
;;(load "color-theme-twilight.el")
;;(color-theme-zenburn)
;;;;(color-theme-twilight)


;;
;; ????
(load-file "~/.emacs.d/lisp/caml.el")

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
;; aspell replaces ispell
(setq-default ispell-program-name "aspell")
;; Default dictionary. To change do M-x ispell-change-dictionary RET.
(setq ispell-dictionary "english")

;;
;; turn on flyspell mode (shows errors as you type)
(flyspell-mode)

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
;; org mode stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

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
;;
;; AUCTeX
;;
;;
(setq TeX-engine 'xetex)                ; use xetex instead of latex
(setq TeX-parse-self t)                 ; enable parsing on load
(setq TeX-auto-save t)                  ; enable parsing on save
(setq TeX-parse-self t)                 
(setq-default TeX-master nil)           ; set up AUCTeX to deal with
                                        ; multiple file documents.
(setq LaTeX-biblatex-use-Biber t)       ; use biber by default
(setq TeX-PDF-mode t)                   ; use pdflatex by default
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode) ;turn on pdf-mode.  (how are these different?)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (TeX-fold-mode 1)))         ;turn on tex-fold-mode by default
;; LaTeX-math-mode http://www.gnu.org/s/auctex/manual/auctex/Mathematics.html
(add-hook 'TeX-mode-hook 'LaTeX-math-mode)

;;; RefTeX
;; Turn on RefTeX for AUCTeX http://www.gnu.org/s/auctex/manual/reftex/reftex_5.html
(require 'reftex)
(add-hook 'TeX-mode-hook 'turn-on-reftex)
(add-hook 'TeX-mode-hook 'flyspell-mode); Enable Flyspell mode for TeX modes such as AUCTeX. Highlights all misspelled words.

;;
;; temp fix.
;; see: http://tex.stackexchange.com/questions/327952/auctex-symbols-function-definition-is-void-signum
;;(require 'cl)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; clojure
;;
;;;;(setq inferior-lisp-program "java -cp /usr/local/clojure/clojure.jar clojure.main")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; scheme: racket/guile/chicken
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
;; paredit (tutotial: http://p.hagelb.org/paredit-outline)
;;(autoload 'paredit-mode "paredit"
;;  "Minor mode for pseudo-structurally editing Lisp code." t)
;;(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
;;(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
;;(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
;;(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))


(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Robe (enhanced Ruby mode)
;; start with
;; M-x run-ruby
;; or 
;; M-x inf-ruby
(add-hook 'ruby-mode-hook 'robe-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; unused or obsolete stuff.......
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;; setup to print to franklin
;;(setq lpr-switches '("-d lj4simx"))

;;
;; end of .emacs file

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (tuareg scala-mode robe rainbow-delimiters quack projectile php-mode perl6-mode paredit markdown-mode+ haskell-mode geiser flycheck fill-column-indicator cider auctex)))
 '(quack-programs
   (quote
    ("racket" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -il r6rs" "mzscheme -il typed-scheme" "mzscheme -M errortrace" "mzscheme3m" "mzschemecgc" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(safe-local-variable-values (quote ((TeX-master . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )







