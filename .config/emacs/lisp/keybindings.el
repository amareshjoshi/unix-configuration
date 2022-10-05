;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; key bindings
;;
;; ~/.emacs.d/lisp/keybindings.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; spelling and dictionary
					; default binding is M-$
(global-set-key (kbd "C-M-z") 'ispell-word)
					; dictionary lookup
(global-set-key (kbd "M-#") 'dictionary-lookup-definition)


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
;; <f1> reserved for help (C-h)
(global-set-key (kbd "<f2>" ) 'shell)
(global-set-key (kbd "<f3>" ) 'eshell)
(global-set-key (kbd "<f4>" ) 'term)
;;
;; used by recent file mode (recentf-mode 1) 
(global-set-key (kbd "C-M-r" ) 'recentf-open-files)

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

