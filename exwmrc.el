;;; EXWM development
(push "~/dev/exwm" load-path)
;; v0.9 - e58ac743f97a005bb737e234a1eb1d467ed84bde
(push "~/dev/xelb" load-path)

;; Enable debugging
(setq exwm-debug-on t)

(defun lg-minibuf-focus-in ()
  (select-frame-set-input-focus (selected-frame) t))

(defun lg-minibuf-focus-out ()
  (exwm-input--update-focus))

;; remove idle timer hackery, minibuffer height won't grow
(remove-hook 'minibuffer-setup-hook 'exwm-layout--on-minibuffer-setup)

(add-hook 'minibuffer-setup-hook 'lg-minibuf-focus-in)
(add-hook 'minibuffer-exit-hook 'lg-minibuf-focus-out)


(defun lg--exwm-client-info ()

;; (define-xwem-command xwem-help-client-info (cl)
;;   "Display information about current client."
;;   (xwem-interactive (list (xwem-cl-selected)))
;;   (xwem-help-display (format "%s" (xwem-client-name cl))
;;     (insert (format (concat "Manage Type: %s\n"
;;                             "Application: %s\n"
;;                             "Uptime: %s\n"
;;                             "\n")
;;                     (upcase (symbol-name (xwem-cl-manage-type cl)))
;;                     (or (car (xwem-client-application cl)) "UNKNOWN")
;;                     (xwem-cl-get-uptime cl)))
;;     (when (xwem-local-map cl)
;;       (xwem-describe-prefix-bindings-1 (xwem-local-map cl) nil
;;         (format "Client local bindings:"))
;;       (insert "\n"))
      
;;     (let ((xwi (shell-command-to-string 
;;                 (format "xwininfo -all -id 0x%x"
;;                         (X-Win-id (xwem-cl-xwin cl))))))
;;       (insert (decode-coding-string xwi (xwem-misc-locale-coding-system)) "\n"))

;;     (let ((xpr (shell-command-to-string
;;                 (format "xprop -id 0x%x"
;;                         (X-Win-id (xwem-cl-xwin cl))))))
;;       (insert (format "xprop: Window id: 0x%x %S\n\n"
;;                       (X-Win-id (xwem-cl-xwin cl))
;;                       (xwem-client-name cl)))
;;       (insert (decode-coding-string xpr (xwem-misc-locale-coding-system)) "\n"))
;;     ))
)


(require 'exwm)
(require 'exwm-wconf)
(exwm-wconf-push)                       ;push initial
(exwm-wconf-tabs-mode 1)
(add-hook 'exwm-manage-finish-hook 'exwm-wconf-push)

(defun lg-wconf-update-p (wconf)
  (let ((sbuf (car (exwm-wconf--selected))))
    (or (not (buffer-live-p sbuf))
        (and (not (eq (buffer-local-value 'major-mode sbuf) 'exwm-mode))
             (not (eq major-mode 'exwm-mode))))))

(setq exwm-wconf-autoupdate-predicate 'lg-wconf-update-p)

(setq exwm-wconf--header-prefix
      '(:eval (format "[%d]" exwm-workspace-current-index)))
(setq exwm-wconf--header-string '(" "))

;; Disable in modeline
(display-time-mode -1)

;; Enable header-line
(with-exwm-wconf-header-line
 (display-time-mode 1))

(when (and (boundp 'battery-status-function)
           battery-status-function)
  (require 'dbus)
  (setq battery-status-function 'battery-upower)
  (setq battery-mode-line-format " [%p%%]  ")

  (with-exwm-wconf-header-line
   (display-battery-mode 1)))

;; Restore wconf on `C-x #'
(add-hook 'server-done-hook 'exwm-wconf-restore-buffer)


(setq exwm-manage-switch-on-maprequest
      #'(lambda (x)
          (exwm-wconf-pop-to-buffer (exwm--id->buffer x))))

(exwm-input-set-key (kbd "H-r") 'exwm-reset)
(exwm-input-set-key (kbd "H-g") 'exwm-input-release-keyboard)
;; Bind a key to switch workspace interactively
;;(exwm-input-set-key (kbd "H-w") 'exwm-workspace-switch)

;; Make X buffers be special
(defun lg-exwm-rename-buffer ()
  (exwm-workspace-rename-buffer (concat " " exwm-class-name ": " exwm-title)))

;; Use class name to name an EXWM buffer
(add-hook 'exwm-update-class-hook 'lg-exwm-rename-buffer)
(add-hook 'exwm-update-title-hook 'lg-exwm-rename-buffer)

;; Always use char mode
(add-hook 'exwm-manage-finish-hook 'exwm-input-release-keyboard)

(setq exwm-workspace-number 1)

(exwm-input-set-key (kbd "H-C-0") 'exwm-workspace-switch-nth)
(exwm-input-set-key (kbd "H-C-1") 'exwm-workspace-switch-nth)
(exwm-input-set-key (kbd "H-C-2") 'exwm-workspace-switch-nth)
(exwm-input-set-key (kbd "H-C-3") 'exwm-workspace-switch-nth)
(exwm-input-set-key (kbd "H-s")
                    #'(lambda ()
                        (interactive)
                        (exwm-workspace-switch 0)))


(exwm-input-set-key (kbd "H-]") 'exwm-wconf-next)
(exwm-input-set-key (kbd "H-[") 'exwm-wconf-prev)
(exwm-input-set-key (kbd "H-.") 'exwm-wconf-next)
(exwm-input-set-key (kbd "H-,") 'exwm-wconf-prev)
(exwm-input-set-key (kbd "H-w") 'exwm-wconf-prev)
(exwm-input-set-key (kbd "H-v") 'exwm-wconf-next)
(exwm-input-set-key (kbd "H-t") 'exwm-wconf-transpose)
(exwm-input-set-key (kbd "H-<return>") 'exwm-wconf-push)
(exwm-input-set-key (kbd "H-<delete>") 'exwm-wconf-remove)
(exwm-input-set-key (kbd "H-<backspace>") 'exwm-wconf-remove)
(exwm-input-set-key (kbd "H-C-l") 'exwm-wconf-other)

(exwm-input-set-key (kbd "H-x H-C-l") 'exwm-wconf-restore-buffer)
(exwm-input-set-key (kbd "H-x H-l") 'exwm-wconf-restore-buffer)
(exwm-input-set-key (kbd "H-l") 'exwm-wconf-restore-buffer)

(exwm-input-set-key (kbd "H-M-x") 'execute-extended-command)


(exwm-input-set-key (kbd "H-x 1") 'delete-other-windows)
(exwm-input-set-key (kbd "H-x 2") 'split-window-below)
(exwm-input-set-key (kbd "H-x 0") 'delete-window)
(exwm-input-set-key (kbd "H-x +") 'balance-windows)
(exwm-input-set-key (kbd "H-n") 'other-window)
(exwm-input-set-key (kbd "H-p") '(lambda (cnt) (interactive "p") (other-window (- cnt))))

;(exwm-input--update-global-prefix-keys)

(fset 'orig-buffer-list (symbol-function 'buffer-list))

(defun exwm-workspace-buffers (&optional frame)
  "Return list of clients for given workspace FRAME."
  (remove-if-not #'(lambda (b)
                     (with-current-buffer b
                       (and (eq major-mode 'exwm-mode)
                            (or (null frame) (eq exwm--frame frame)))))
                 (orig-buffer-list)))

(defun lg-ido-switch-app (buf)
  "Switch to application."
  (interactive
   (list (get-buffer (ido-completing-read
                      "X application: "
                      (mapcar 'buffer-name (exwm--x-list '(exwm-mode))) nil t))))
  (exwm-wconf-pop-to-buffer buf))

(exwm-input-set-key (kbd "H-x b") 'lg-ido-switch-app)
(exwm-input-set-key (kbd "H-x H-b") 'ido-switch-buffer)
(exwm-input-set-key (kbd "H-:") 'eval-expression)
(exwm-input-set-key (kbd "H-C-x") 'execute-extended-command)
(exwm-input-set-key (kbd "H-M-x") 'execute-extended-command)
(exwm-input-set-key (kbd "H-!") 'shell-command)
(exwm-input-set-key (kbd "H-#") 'lg-mini-calc)

(exwm-input-set-key (kbd "M-:") 'eval-expression)
(exwm-input-set-key (kbd "H-:") 'eval-expression)
(exwm-input-set-key (kbd "H-C-x") 'execute-extended-command)
(exwm-input-set-key (kbd "H-M-x") 'execute-extended-command)

(defun lg-exwm-start-xterm-screen ()
  (interactive)
  (start-process "" nil "xterm" "-lc" "-e" "screen" "-RR"))

(defun lg-exwm-start-xterm ()
  (interactive)
  (start-process "" nil "xterm" "-lc"))

(defun lg-exwm-start-firefox ()
  (interactive)
  (start-process "" nil "firefox"))

(defun lg-exwm-start-xlock ()
  (interactive)
  (start-process "" nil "xlock" "-mode" "eyes"))

(defun lg-exwm-start-opera ()
  (interactive)
  (start-process "" nil "opera"))

(exwm-input-set-key (kbd "H-a X") 'lg-exwm-start-xterm-screen)
(exwm-input-set-key (kbd "H-a x") 'lg-exwm-start-xterm)
(exwm-input-set-key (kbd "H-a f") 'lg-exwm-start-firefox)
(exwm-input-set-key (kbd "H-a l") 'lg-exwm-start-xlock)
(exwm-input-set-key (kbd "H-a o") 'lg-exwm-start-opera)

(defun lg-lupe-geometry ()
  (let ((dw (x-display-pixel-width)))
    (cond ((<= dw 1920) "520x110+200+700")
          ((<= dw 2560) "720x110+200+1000")
          (t "520x110+200+700"))))

(defun lg-xwem-lupe-in ()
  "Start lupe."
  (interactive)
  (start-process "" nil
                 "lupe" "-override_redirect" "-font" "10x20" "-noshape" "-nohud" "-mag" "3"
                 "-geometry" (lg-lupe-geometry))
  (exwm-input-set-key (kbd "H-+") 'lg-xwem-lupe-out))

(defun lg-xwem-lupe-out ()
  "Kill lupe."
  (interactive)
  (start-process "" nil "killall" "lupe")
  (exwm-input-set-key (kbd "H-+") 'lg-xwem-lupe-in))

(exwm-input-set-key (kbd "H-+") 'lg-xwem-lupe-in)

;; Universal argument
(exwm-input-set-key (kbd "H-1") 'digit-argument)
(exwm-input-set-key (kbd "H-2") 'digit-argument)
(exwm-input-set-key (kbd "H-3") 'digit-argument)
(exwm-input-set-key (kbd "H-4") 'digit-argument)
(exwm-input-set-key (kbd "H-5") 'digit-argument)
(exwm-input-set-key (kbd "H-6") 'digit-argument)
(exwm-input-set-key (kbd "H-7") 'digit-argument)
(exwm-input-set-key (kbd "H-8") 'digit-argument)
(exwm-input-set-key (kbd "H-9") 'digit-argument)
(exwm-input-set-key (kbd "H-0") 'digit-argument)
(exwm-input-set-key (kbd "H-u") 'universal-argument)

(exwm-input-set-key (kbd "H-f") 'exwm--forward-app)
(exwm-input-set-key (kbd "H-b") 'exwm--backward-app)

(defun lg--exwm-multitran (word)
  "Check primary selection for the current word."
  (interactive (list (gui-selection-value)))
  (if word
      (with-temp-buffer
        (insert word)
        (call-interactively 'multitran))
    (call-interactively 'multitran)))

(exwm-input-set-key (kbd "H-c d") 'lg--exwm-multitran)

;;;
(defun lg--exwm-kbd-quit ()
  "Make focus follow mouse."
  (interactive)
  (xcb:+request exwm--connection
      (make-instance 'xcb:SetInputFocus
                     :revert-to xcb:InputFocus:PointerRoot
                     :focus xcb:InputFocus:PointerRoot
                     :time xcb:Time:CurrentTime))
  (xcb:flush exwm--connection))


(exwm-input-set-key (kbd "H-G") 'lg--exwm-kbd-quit)


(defun lg-exwm-unmanage-all ()
  (mapc (lambda (e)
          (let ((win (car e)))
            (exwm-manage--unmanage-window win t)
            (xcb:+request exwm--connection
                (make-instance 'xcb:MapWindow :window win))
            (xcb:flush exwm--connection)))
        exwm--id-buffer-alist))

(add-hook 'kill-emacs-hook 'lg-exwm-unmanage-all)

;; exwmrc.el ends here
