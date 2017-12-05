;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;
;; special .emacs file for MSU public lab machines
;;
;; should work across platforms (linux and mac)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; elisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; add my own elisp directory to the loadpath
(add-to-list 'load-path "~/.emacs.d/lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; platform specific stuff (linux, apple, ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (eq system-type 'darwin) ;; mac specific settings
  ;;
  ;; set PATH and exec-path
  (setq path (concat
              "/Users/joshia/bin:"
              "/usr/local/sbin:/usr/local/bin:"
              "/bin:/usr/bin:"
              "/sbin:/usr/sbin"))
  (setenv "PATH" path)
  (set-default-font "Monaco-14")
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
  )

(when (eq system-type 'gnu/linux) ;; linux specific settings
  ;;
  ;; dired settings
  ;; linux "ls" uses the --dired option
  )


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; terminal vs gui, other
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond ((display-graphic-p)
       (menu-bar-mode t)
       ;;
       ;; get rid of scroll and tool bar
       (tool-bar-mode -1)
       (scroll-bar-mode -1)
       ;;
       ;; ruler mode
       ;; doesn't work. needs to be turned on for each mode i think
       ;;  (ruler-mode t)
       )
      ;;
      ;; terminal stuff
      (t 
       (menu-bar-mode -1)
       )
)
;;
;; set size dynamically
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 160 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 160))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 200)
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)
;;
;; set position (this needs some tweaking)
(if window-system
    (progn
      (add-to-list 'default-frame-alist (cons 'top 50))
      (add-to-list 'default-frame-alist (cons 'left 50))))

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
;; always split windows horizontally
(setq split-height-threshold 80)
(setq split-width-threshold nil)

;;
;; force emacs to insert tabs instead of spaces
(setq-default indent-tabs-mode nil)
;;
;; display tab characters as caret-I
;;(standard-display-ascii ?\t "^I")

;;
;; set the title bar to show file name if available, buffer name otherwise
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;;
;; improves display performance, maybe ...
;; https://masteringemacs.org/article/improving-performance-emacs-display-engine
(setq redisplay-dont-pause t)

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
;; dired options
(setq delete-by-moving-to-trash t)

;;
;; don't make #backups# if choose not to save a file
;; also don't make 'dead' pointers if a session quits suddenly
;;
;; (doesn't work!)
(setq auto-save-list-file-prefix nil) 

;;
;; fix cut and paste in X
;; see http://emacswiki.org/emacs/CopyAndPaste
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)
;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
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
;; red cursors are faster
(set-cursor-color "#ff0000")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; keymaps
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
(global-set-key (kbd "M-h") 'global-hl-line-mode) ; toggle global line highlight mode
;;(global-set-key "\M-." 'set-mark-command)
(global-set-key (kbd "C-M-z") 'ispell-word)

;;
(global-set-key (kbd "M-r" ) 'replace-regexp)
(global-set-key (kbd "M-s" ) 'replace-string)
;;
;; remap other-window
(global-set-key (kbd "M-o" ) 'other-window)
;;
;; eshell and (r(ecursive))grep
(global-set-key (kbd "<f2>" ) 'other-window) ;; avoid using f1
(global-set-key (kbd "<f3>" ) 'shell)
(global-set-key (kbd "<f4>" ) 'eshell)
(global-set-key (kbd "<s-f12>" ) 'a2ps-buffer)
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

;; set indentation for python
(setq python-indent-offset 4)

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
;; answer just y/n to to yes/no question prompts
;;
;;(defalias 'yes-or-no-p 'y-or-n-p)

;;
;; build and test RE's on the fly
;; https://masteringemacs.org/article/re-builder-interactive-regexp-builder
(require 're-builder)
(setq reb-re-syntax 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; eshell stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))
;;
;; we need to make sure we don't add a extra separter at the end
(defun join (list &optional sep)
  "Concatenate elements of a list into string with an optional separator."
  (progn
                                        ; default separator is space
    (setq s (if sep sep " "))
    ;; if the list is just 1 element return it
    ;; else concatenate the car with the rest of the list
    (if (= (length list) 1)
        (car list)
      (concat (car list) s (join (cdr list) s)))
    )
  )

;;
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
     ;;
     ;; just the host part of the fqdn
     (car (split-string (shell-command-to-string "hostname") "[.\n]"))
     ;; fqdn, but trim the \n
     ;;(substring (shell-command-to-string "hostname") 0 -1)
     ": "
     ;; entire path
     ;;(with-face (concat (eshell/pwd) " ") :background header-bg)
     ;;(with-face (concat (eshell/pwd) " ") :foreground "green")
     ;; just the last item in the path
     ;;(with-face (car (last (eshell-split-path (eshell/pwd)))) :foreground "green")
     ;; first and last elements in the path
     (with-face
      (join (append (list (car (cdr (split-string (eshell/pwd) "/"))))
                    (last (split-string (eshell/pwd) "/")))
            "/.../")
      :foreground "green")
     (if (= (user-uid) 0)
         (with-face " #" :foreground "red")
       " $")
     " ")))
(setq eshell-prompt-function 'arj-eshell-prompt)
(setq eshell-highlight-prompt nil)
;;(setq eshell-highlight-prompt 1)

;;
;; end of .emacs file

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
