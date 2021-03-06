;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;
;; .emacs file for Amaresh Joshi
;;
;; should work across platforms (linux,  mac and windows)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; emacs server
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; start emacs server
;;(require 'server)
;;(server-start)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; elisp and packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; add my own elisp directory to the loadpath

(add-to-list 'load-path "~/.emacs.d/lisp")
;;
;; this is to exclude old elisp files that cause problems with later versions of emacs
(when (<= emacs-major-version 24) 
  (add-to-list 'load-path "~/.emacs.d/lisp-old")
  )
;;
;; utility functions
(load "utility-functions.el")

;;
;; MSU Commons functions and defs
(load "msucommons.el")

;;
;; load packages for emacs version >= 25
(when (>= emacs-major-version 25) 
  (load "load-packages.el")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; platform specific stuff (linux, apple, ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; but first get the right version of TeX
(setenv "TEXBINPREFIX" "x86_64")
(if (file-directory-p "/usr/local/texlive/2018")
    (setenv "TEXYEAR" "2018")
  )
(if (file-directory-p "/usr/local/texlive/2019")
    (setenv "TEXYEAR" "2019")
  )
(if (file-directory-p "/usr/local/texlive/2020")
    (setenv "TEXYEAR" "2020")
  )
(if (file-directory-p "/usr/local/texlive/2021")
    ;; need to do 2 things
    ;; (if ...) only does 1 expression
    ;; progn is a way of doing multiple expressions in one
    (progn (setenv "TEXYEAR" "2021")
           (if (eq system-type 'darwin)
               (setenv "TEXBINPREFIX" "universal")
             )
           
           )
  )
;;
;; mac/apple/darwin
(when (eq system-type 'darwin)
  (setenv "TEXBIN" (concat "/usr/local/texlive/"
                           (getenv "TEXYEAR")
                           "/bin/"
                           (getenv "TEXBINPREFIX")
                           "-darwin")
          )
  ;;
  ;; set PATH and exec-path
  (setq path (concat
              "/Users/joshia/bin:"
              (getenv "TEXBIN") ":"
              ;;put GNU coreutils before BSD
              "/usr/local/opt/coreutils/libexec/gnubin"
              ;; macports (to be removed)
              ;;"/Applications/MacPorts/Emacs.app/Contents/MacOS/bin:"
              ;;"/opt/local/libexec/gnubin:/opt/local/bin:/opt/local/sbin:"
              ;; /usr/local is where where docker, vbox, etc. get installed
              "/usr/local/bin:/usr/local/sbin:"
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
  (setq scheme-program-name "/usr/local/bin/guile")
  ;;
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
  (setq dired-use-ls-dired nil)
  ;;
  ;; for codeacademy courses that use python2
  ;;(setq python-shell-interpreter "python2")
  (setq python-shell-interpreter "python3")
  )
;;
;; GNU linux
(when (eq system-type 'gnu/linux)
  (setenv "TEXBIN" (concat "/usr/local/texlive/"
                           (getenv "TEXYEAR")
                           "/bin/x86_64-linux"))
  ;;
  ;; set PATH and exec-path
  (setq path (concat
              (getenv "TEXBIN") ":"
              "/usr/local/java/bin:"
              "/home/joshia/bin:"
              "/usr/local/sbin:/usr/local/bin:"
              "/usr/sbin:/usr/bin:"
              "/sbin:/bin:"
              "/usr/local/games:/usr/games"))
  (setenv "PATH" path)
  ;;
  ;; exec-path is a list of directories
  (setq exec-path (append (split-string path ":")))
  ;;(setq exec-path (append '("/foo/bar/bin")))
  ;;
  ;; dired settings
  ;; linux "ls" uses the --dired option
  (setq dired-use-ls-dired t)
  ;;
  (setq scheme-program-name "/usr/bin/guile3.0")
  (setq geiser-guile-binary "guile3.0")
  ;;
  ;;

  ;; we have to initialize this variable because it may not be set yet
  ;; when this code firest runs. this  is okay because values will be added to it later
  (setq org-file-apps ())
  ;;
  ;; this is only for WSL 
  ;; set the PDF viewer for org latex export
  ;; this gets a little weird because we are working across windows and linux
  ;; and windows programs don't understand linx file paths (at all)
  ;; linux programs only access windows files through "/mnt/c/..."
  ;; - for the linux version we want to exclude the "/home/joshia/" part of the file name
  ;;   and in windows replace it with "C:\Users\joshia\"
  ;; - we need to escape (or double escpe all the special characaters (spaces, parenthesis, backslashes, etc.)
  ;; WSL
  ;; (add-to-list 'org-file-apps
  ;;              '("/home/joshia/\\(.+\\.pdf\\)" .

  ;;
  ;; regular linux
  (add-to-list 'org-file-apps '("pdf" . "evince %s"))
  ;;
  )
;;
;; MS windows
(when (eq system-type 'windows-nt)
  ;; win stuff
  ;;
  ;; powershell stuff
  ;;(setq explicit-shell-file-name "C:\\Windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe")
  ;;(setq explicit-powershell.exe-args '("-InputFormat" "Text" "-OutputFormat" "Text"))
  ;;
  ;; use wsl as the shell
  (setq explicit-shell-file-name "bash.exe")
  (setq explicit-powershell.exe-args '())
  (setq shell-file-name explicit-shell-file-name)
  ;; not sure about this require
  (require 'powershell)
  ;;
  (setq scheme-program-name "C:\\Programs\\Racket\\no-racket-for-mswindows-Racket.exe")
  ;;
  (setq TeX-output-view-style             ; default viewers for AUCTeX
        '(("^pdf$" "." "C:\\Program Files (x86)\\Adobe\\Acrobat 11.0\\Acrobat\\Acrobet.exe")
          ))
  )
;;
;; cygwin
(when (eq system-type 'cygwin) 
  ;; cygwin
  ;;
  (setq scheme-program-name "/usr/local/bin/no-racket-for-cygwin")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; startup settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; clean and quiet startup
(setq inhibit-startup-screen t)
(setq initial-scratch-message "the way is void...")
(setq visible-bell nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; utf-8 encoding
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; prevent shells from echoing their arguments (lines)
(defun my-comint-init ()
  (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'my-comint-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; turn on semantic mode (NEW, see docs)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(semantic-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; terminal vs gui, other
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond ((display-graphic-p)
       ;;
       ;; keep this off by default
       ;; turn it on for auctex
       ;; (menu-bar-mode t)
       (menu-bar-mode -1)
       ;;
       ;; get rid of scroll and tool bar
       (tool-bar-mode -1)
       (scroll-bar-mode -1)
       ;;
       ;; ruler mode
       ;; doesn't work. needs to be turned on for each mode i think
       ;;  (ruler-mode t)
       ;;
       ;; uses Unicode symbols for stuff like arrows, Greek letters 
       (prettify-symbols-mode)
       )
      ;;
      ;; terminal stuff
      (t 
       (menu-bar-mode -1)
       ;; use mouse in terminals - nice but doesn't allow copying text
       ;;(xterm-mouse-mode t)
       )
)
;;
;; set size dynamically
;;
;; for small displays make the window 2/3 (0.66) of 
;; the dimensions of the screen.
;; but for large (e.g. 4k) displays make the width
;; 1/2 the width of the screen
;;
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if (display-graphic-p)
      (let* (
            ;;
            ;; slightly smaller than the screen 
            (OFFSET 75)
            (HEIGHT (/ (- (display-pixel-height) OFFSET) (frame-char-height)))
            ;;
            ;; for really big displays use a relative size (2000)
            (LARGE 1100)
            (WIDTH  (if (< (display-pixel-height) LARGE)
                        ;; not LARGE (laptop)
                        (/ (/ (* (display-pixel-width) 2) 3) (frame-char-width))
                      ;; LARGE (4k)
                      (/ (/ (display-pixel-width) 2) (frame-char-width)))
                    )

            )
        (progn
          (add-to-list 'default-frame-alist 
                       (cons 'height 40     ; HEIGHT
                             ))
          (add-to-list 'default-frame-alist 
                       (cons 'width 100     ; WIDTH
                             ))
          ;;
          ;; for now set font size based ONLY on pixel width for
          ;; graphical environments.
          ;; font for terminals will come from the terminal settings
          (if (< (display-pixel-height) 1100)
              ;; laptop or small screen (1050 or 1080)
              (set-frame-font "Source Code Pro-15" t t)
            ;; big screen (4k)
            (set-frame-font "Source Code Pro-16" t t))
          ))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the problem is with a horizontal+vertical two monitor combo
;; the pixel height and width are the maximum's from each monitor
;; i.e. 1920x1080 + 1080x1920 gives the following: width = 3000, height = 1920
;;
;; it may be possible to use (display-monitor-attributes-list)
;; to better determine the ``best'' window size
;; for now only set the window size if the width is NOT "too big"
;;
;; for mixed displays (width > 2000) use batch files with "emacs -g 200x64 ..."
;;
;; (if (< (display-pixel-width) 1920)
;;     (set-frame-size-according-to-resolution)
;; )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; set size
(set-frame-size-according-to-resolution)
;;
;; set position
(if (display-graphic-p)
    (progn
      (add-to-list 'default-frame-alist (cons 'top 50))
      (add-to-list 'default-frame-alist (cons 'left 50))))

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
(global-hl-line-mode nil)

;;
;; always split windows horizontally
(setq split-height-threshold 80)
(setq split-width-threshold nil)

;;
;; force emacs to insert tabs instead of spaces
(setq-default indent-tabs-mode nil)
;; and convert tabs to spaces when saving files
;; if indent-tabs-mode is off, untabify before saving
;; see: https://www.emacswiki.org/emacs/UntabifyUponSave
(add-hook 'write-file-hooks 
          (lambda () (if (not indent-tabs-mode)
                         (untabify (point-min) (point-max)))
            nil ))
;;
;; display tab characters as caret-I
;;(standard-display-ascii ?\t "^I")

;;
;; enable narrowing commands (C-x n n  and C-C n w (widen))
(put 'narrow-to-region 'disabled nil)

;;
;; set the title bar to show file name if available, buffer name otherwise
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;;
;; improves display performance, maybe ...
;; https://masteringemacs.org/article/improving-performance-emacs-display-engine
(setq redisplay-dont-pause t)

;;
;; buffers with these names will open sepearte windows (frames) in gui
(setq special-display-buffer-names
      '("*Colors*" "*Faces*"))

;;
;;  make no backups
(setq backup-inhibited t)
(setq version-control ())
(setq make-backup-files ())

;;
;; dired options
(setq delete-by-moving-to-trash t)

;;
;; set grep command and args
;; gives an error in WSL
;; (grep-apply-setting 'grep-command "grep --ignore-case --color -nH -e")


;;
;; don't make #backups# if choose not to save a file
;; also don't make 'dead' pointers if a session quits suddenly
;;
;; (doesn't work!)
;;(setq auto-save-list-file-prefix nil) 

;;
;; fix cut and paste in X
;; see http://emacswiki.org/emacs/CopyAndPaste
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)
;; Save whatever???s in the current (system) clipboard before
;; replacing it with the Emacs??? text.
;; https://github.com/dakrone/eos/blob/master/eos.org
(setq save-interprogram-paste-before-kill t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; coloring
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; colors/themes
;;
;; custom-theme only works with emacs24 or higher
(when (>= emacs-major-version 24) 
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  ;;(load-theme 'zenburn t)
  ;; (load-theme 'modus-operandi)
  
  ;;(load-theme 'adwaita t)
  ;;(load-theme 'deeper-blue t)
  ;;(load-theme 'dichromacy t)
  ;;(load-theme 'leuven t)
  ;;(load-theme 'light-blue t)
  ;;(load-theme 'manoj-dark t)
  (load-theme 'misterioso t)
  ;;(load-theme 'tango t)
  ;;(load-theme 'tango-dark t)
  ;;(load-theme 'tsdh-dark t)
  ;;(load-theme 'tsdh-light t)
  ;;(load-theme 'wheatgrass t)
  ;;(load-theme 'whiteboard t)
  ;;(load-theme 'wombat t)
  )
;;
;; older emacs need to load the color-theme (color NOT *custom*) package manually
(when (<= emacs-major-version 23) 
  (require 'color-theme)
                                        ; lighter theme
  (color-theme-aalto-light)
  ;;(color-theme-aalto-gnome)
  ;;(color-theme-aalto-gnome2) ; dark green
  )
;;
;; red cursors are faster
(set-cursor-color "#ff0000")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; spell check options
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; aspell replaces ispell
(setq-default ispell-program-name "aspell")
;; Default dictionary. To change do M-x ispell-change-dictionary RET.
(setq ispell-dictionary "english")
;;
;; turn on flyspell mode (shows errors as you type)
(flyspell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; aliases
;;
;; useful to shorten commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defalias 'vhc 'visit-humcore)
(defalias 'qrr 'query-replace-regexp)
(defalias 'skr 'show-kill-ring)
(defalias 'rfb 'rename-file-and-buffer)
;; answer just y/n to to yes/no question prompts
;;(defalias 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; key bindings
;;
;; always use "kbd" to set keys (see Xah Emacs site)
;;      (kbd "M-f") vs "\M-f"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; can use <fN> to define fuction keys
;; on apple keyboard need to press "fn" to use function keys
;; windows/apple-command key is super
;;
;; define a global or particular vkey map as follows:
;;     (define-key global-map (kbd "M-.") 'set-mark-command)
;;     (define-key text-mode-map (kbd "M-r") 'replace-regexp)
;;
;;

;;
;; better bindings for M-x
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)


;; C-o (default open-line) will be the prefix key for tmux
(global-unset-key (kbd "C-o"))

;;
;; keybindings for windmove
;; M-arrow_keys will move around windows
(global-set-key (kbd "<M-up>") 'windmove-up)
(global-set-key (kbd "<M-down>") 'windmove-down)
(global-set-key (kbd "<M-left>") 'windmove-left)
(global-set-key (kbd "<M-right>") 'windmove-right)

;;
(global-set-key (kbd "M-h") 'global-hl-line-mode) ; toggle global line highlight mode
;;(global-set-key "\M-." 'set-mark-command)
;;
;; 
(global-set-key (kbd "C-M-z") 'ispell-word)

;;
;; both C-/ and C-M-/ both don't work in some terminal emulators (gnome-terminal and foot)
(global-set-key (kbd "C-M-u") 'undo)
(global-set-key (kbd "C-x u")
                (lambda ()
                  (interactive)
                  (message "Use C-M-u to undo.")))
;;
(global-set-key (kbd "M-r" ) 'replace-regexp)
(global-set-key (kbd "M-s" ) 'replace-string)
;;
;; eshell and (r(ecursive))grep
(global-set-key (kbd "<f2>" ) 'shell)
(global-set-key (kbd "<f3>" ) 'eshell)
(global-set-key (kbd "<f4>" ) 'term)

;;
;;
;; Windows Terminal (Preview) specific stuff
;;
;; unbind these to use as cut/paste in windows terminal
(global-unset-key (kbd "C-M-c"))
(global-unset-key (kbd "C-M-v"))
;;
;; because of a bug in C-M-z is treated as C-z
;; as a (hopefully temporary) fix we'll unbind C-z from suspend-emacs
;; and add another keybinding for ispell-word
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-M-y") 'ispell-word)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; avoid printing by mistake in osX with command-p 
;;default: (global-set-key (kbd "S-p" ) 'ns-print-buffer)
;;(global-unset-key (kbd "S-p" ))
;;(global-set-key (kbd "S-p" ) 'shell)
;;(global-unset-key "\S-p")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; keyboard prefixes and bindings
;;
;; prefix "C-c e" already exists, so don't need to define it
(global-set-key (kbd "C-c e b") 'do-eval-buffer)
(global-set-key (kbd "C-c e e") 'toggle-debug-on-error)
(global-set-key (kbd "C-c e f") 'emacs-lisp-byte-compile-and-load)
(global-set-key (kbd "C-c e r") 'eval-region)
(global-set-key (kbd "C-c e s") 'scratch)
;;
;; markdown -> pdf
(global-set-key (kbd "C-c e p") 'markdown-export-pandoc-pdf)
;;
;; run asscociated (external) application
(global-set-key (kbd "C-c e l") 'launch-files-dired)


;;
;; define new prefix "C-h e" and its keybinding
(define-prefix-command 'ctl-h-e-prefix)
(global-set-key (kbd "C-h e") 'ctl-h-e-prefix)
;;
;; now add keys to this prefix-key map
;;
;; the old mapping for "C-h e"
(define-key ctl-h-e-prefix (kbd "e") 'view-echo-area-messages)
(define-key ctl-h-e-prefix (kbd "f") 'find-function)
(define-key ctl-h-e-prefix (kbd "k") 'find-function-on-key)
(define-key ctl-h-e-prefix (kbd "l") 'find-library)
(define-key ctl-h-e-prefix (kbd "v") 'find-variable)
(define-key ctl-h-e-prefix (kbd "V") 'apropos-value)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; indentation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; python settings
;; set indentation for python
(setq python-indent-offset 2)
(setq python-shell-completion-native-disabled-interpreters '("python3"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; language specific packages and settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; the string called when compile is called
(setq compile-command "make")

;;
;; sigh ...
(setq gnus-default-nntp-server "news.eternal-september.org")

;;
;; ????
;;(load-file "~/.emacs.d/lisp/caml.el")


;;
;; eshell stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "eshell-config.el")
(setq eshell-highlight-prompt nil)
;;(setq eshell-highlight-prompt 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; AUCTeX and RefTeX (moved into lisp/load-packages.el)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; clojure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; don't need to set anything


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Robe (enhanced Ruby mode)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; start with
;; M-x run-ruby
;; or 
;; M-x inf-ruby
(add-hook 'ruby-mode-hook 'robe-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   '(yaml-mode web-mode use-package smartparens rainbow-delimiters projectile markdown-mode magit launch graphql-mode graphql-doc graphql gnu-elpa-keyring-update geiser-guile flycheck cider cdlatex auctex adoc-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
