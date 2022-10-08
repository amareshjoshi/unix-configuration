;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; functions for setting window size and position, fonts and colors
;;
;; ~/.config/emacs/lisp/load-packages.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; set colors
(defun set-colors()
  (interactive)
  ;; nice light theme
  ;; leave OFF for now
  ;;(load-theme 'leuven t)

  ;;
  ;; red cursors are faster
  (set-cursor-color "#ff0000")
  ;;
  ;; slightly darker background
  ;; beige #ffe7ba
  (set-background-color "linen")
  ;;(set-background-color "beige")
  ;;(set-background-color "lavender")
)

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
;; set window size and font based on screen resolution
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
					; slightly smaller than the screen
             (OFFSET 75)
             (HEIGHT (/ (- (display-pixel-height) OFFSET) (frame-char-height)))
					; for really big displays use a relative size (2000)
            (LARGE 1100)
            (WIDTH  (if (< (display-pixel-height) LARGE)
					; not LARGE (laptop)
                        (/ (/ (* (display-pixel-width) 2) 3) (frame-char-width))
					; LARGE (4k)
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
	  (setq my-frame-font "Courier")
					; for now set font size based ONLY on pixel width for graphical environments.
					; font for terminals will come from the terminal settings
          (if (< (display-pixel-height) 1100)
					; laptop or small screen (1050 or 1080)
	      (setq my-frame-font "Source Code Pro-16")
					; (else) big screen (4k)
	    (setq my-frame-font "Source Code Pro-16"))
					; set a default font
	  (if (not (x-list-fonts my-frame-font))
	      (setq my-frame-font "Courier"))
	  (progn ; don't know which one of these is needed
	    ;;(add-to-list 'default-frame-alist '(font . my-frame-font))
	    (car '(a b))
	    (set-face-attribute 'default nil :font my-frame-font)))
          )))
