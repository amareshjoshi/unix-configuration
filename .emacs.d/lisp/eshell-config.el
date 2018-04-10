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
