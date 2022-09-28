;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;
;; ~/.config/emacs/init.el
;;
;; should work across platforms (linux, mac and wsl (maybe ms windows))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; personal info
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Amaresh R Joshi"
      user-mail-address "joshia@msu.edu")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; paths and packages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add elisp directory to the loadpath
(add-to-list 'load-path "~/.config/emacs/lisp")
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
;;
;; ELPA and NonGNU ELPA are default in Emacs28
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))
(package-initialize)
(load "upgrade-packages.el")
(load "load-packages.el")
;; run this manually periodically
;;(package-autoremove)
;;-----------------------------------------------------------

;;
;; key bindings
(load "keybindings.el")

;;
;; utility functions
(load "utility-functions.el")

;;
;; MSU Commons functions and defs
(load "msucommons.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; initialize
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; startup settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-screen t)
(setq initial-scratch-message "(car (cdr (cdr (cdr (quote (the way is void))))))")
(setq visible-bell t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; mode line and display stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq display-time-day-and-date t)
;; (display-time)
(line-number-mode t)
(column-number-mode t)
;; puts line numbers in all buffers
(global-display-line-numbers-mode -1)
(global-hl-line-mode -1)
;; set the title bar to show file name if available, buffer name otherwise
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; spaces not tabs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; force emacs to insert tabs instead of spaces
(setq-default indent-tabs-mode -1)
;; and convert tabs to spaces when saving files
;; if indent-tabs-mode is off, untabify before saving
;; see: https://www.emacswiki.org/emacs/UntabifyUponSave
(add-hook 'write-file-hooks 
          (lambda () (if (not indent-tabs-mode)
                         (untabify (point-min) (point-max)))
            nil ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; eshell stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "eshell-config.el")
(setq eshell-highlight-prompt nil)
;;(setq eshell-highlight-prompt 1)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; themes
;;
;; more modus themes info:
;; https://systemcrafters.net/emacs-from-scratch/the-modus-themes/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modus-* themes seem to be buggy
					; border: borderless, 3d, moody
(setq modus-themes-mode-line '(accented borderless))
					; region: accented, bg-only, no-extend
(setq modus-themes-region '(accented bg-only))
					; extra font options
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
					; completion coloring: minimal, opinionated)
(setq modus-themes-completions 'minimal)
					; org customizarions
(setq modus-themes-org-blocks 'gray-background)
					;(setq modus-themes-org-blocks 'tinted-background)
					; modus light
;;(load-theme 'modus-operandi t)
					; modus dark
;;(load-theme 'modus-vivendi t)
;;(load-theme 'adwaita t)
;;(load-theme 'deeper-blue t)
;;(load-theme 'dichromacy t)
(load-theme 'leuven t)
;;(load-theme 'light-blue t)
;;(load-theme 'manoj-dark t)
;;(load-theme 'misterioso t)
;;(load-theme 'tango t)
;;(load-theme 'tango-dark t)
;;(load-theme 'tsdh-dark t)
;;(load-theme 'tsdh-light t)
;;(load-theme 'wheatgrass t)
;;(load-theme 'whiteboard t)
;;(load-theme 'wombat t)

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
(setq ispell-dictionary "en_US")
;;
;; turn on flyspell mode only for certain modes
;; text mode hook will work for many modes
;; - LaTeX
;; - markdown
;; - others?
(add-hook 'text-mode-hook 'flyspell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; aliases
;;
;; useful to shorten commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
					; safely kill emacs server
(defalias 'sbke 'save-buffers-kill-emacs)
					; company manual completion
(defalias 'cc 'company-complete)
					; breakup long lines
(defalias 'fr 'fill-region)
(defalias 'mtt 'modus-themes-toggle)
(defalias 'vhc 'visit-humcore)
(defalias 'qrr 'query-replace-regexp)
(defalias 'skr 'show-kill-ring)
(defalias 'rfb 'rename-file-and-buffer)
(defalias 'plp 'package-list-packages)
                                        ; scheme ide
(defalias 'rg 'run-geiser)
                                        ; clojure ide
(defalias 'cji 'cider-jack-in)

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
;; the string called when compile is called
(setq compile-command "make")
;; python interpreter
(setq python-shell-completion-native-disabled-interpreters '("python3"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; terminal vs gui, other
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tool-bar-mode -1)
(cond ((display-graphic-p)
					; graphics stuff
					; menu bar
       (menu-bar-mode t)
					; no scroll bar
       (scroll-bar-mode -1)
					; uses Unicode symbols for stuff like arrows, Greek letters 
       (prettify-symbols-mode)
       )
      ;; terminal stuff
      (t
					; menu bar in text terminals
       (menu-bar-mode -1)
					; text menu colors
					; use M-x list-face-display to view all faces
       (set-face-attribute 'menu nil
                    :inverse-video nil
                    :background "darkslateblue"
                    :foreground "gray"
                    :bold t)
       (set-face-attribute 'tty-menu-enabled-face nil
                    :inverse-video nil
                    ;;:background "darkslateblue"
                    :foreground "white"
                    :bold t)
       (set-face-attribute 'tty-menu-disabled-face nil
                    :inverse-video nil
                    ;;:background "darkslateblue"
                    :foreground "darkgray"
                    :bold t)
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
	      (progn ;; don't know which one of these is neede
		(add-to-list 'default-frame-alist '(font . "Source Code Pro-15"))
		(set-face-attribute 'default t :font "Source Code Pro-15"))
            ;; big screen (4k)
	    (progn ;; don't know which one of these is neede
	      (add-to-list 'default-frame-alist '(font . "Source Code Pro-16"))
	      (set-face-attribute 'default t :font "Source Code Pro-16")))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; misc
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enable the {up|down}case region commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; always split windows horizontally
(setq split-height-threshold 80)
(setq split-width-threshold nil)
;;
;; enable narrowing commands (C-x n n  and C-C n w (widen))
(put 'narrow-to-region 'disabled nil)

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
;; remember recent files (need to readup on this)
(recentf-mode 1)
;; displays recent files
;; also remaped to C-M-r (see key section)
(defalias 'rof 'recentf-open-files)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;;
;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;;
;; remember mini-buffer history. navigate it with M-n M-p
(setq history-length 25)
(savehist-mode 1)

;; set grep command and args
;; gives an error in WSL
;; (grep-apply-setting 'grep-command "grep --ignore-case --color -nH -e")

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
;; org mode
;;
;; org-babel (built in)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(org-babel-do-load-languages
 '  org-babel-load-languages
    '((emacs-lisp . t)
      (python . t)
      (shell . t)
      (scheme . t)
      (lisp . t)
      (clojure . t)
      ;; Include other languages here...
      ))
					; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)
					; font coloring in src blocks
(setq org-src-fontify-natively t)
					; tabs act like their src code language
(setq org-src-tab-acts-natively t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; platform specific stuff (linux, apple, ...)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; TeX version
(if (eq system-type 'gnu/linux)
    (setenv "TEXBINPREFIX" "x86_64")
  )
(if (eq system-type 'darwin)
    (setenv "TEXBINPREFIX" "universal")
  )
(if (string-match "-[Mm]icrosoft" operating-system-release)
    ;; WSL: WSL1 has "-Microsoft", WSL2 has "-microsoft-standard"
    (setenv "FOO" "win32")
  )

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
    (setenv "TEXYEAR" "2021")
  )
(if (file-directory-p "/usr/local/texlive/2022")
    (setenv "TEXYEAR" "2022")
  )
(if (file-directory-p "/usr/local/texlive/2023")
    (setenv "TEXYEAR" "2023")
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
              "/usr/local/opt/coreutils/libexec/gnubin:"
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
  (setq scheme-program-name "C:\\Programs\\Racket\\no-racket-for-mswindows.exe")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; emacs server
;; do this at end so all configs are completed
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (server-running-p)
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; unused or obsolete stuff.......
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sigh ...
;; (setq gnus-default-nntp-server "news.eternal-september.org")
;;
;; setup to print to franklin
;; (setq lpr-switches '("-d lj4simx"))

;;
;; end of .emacs file
