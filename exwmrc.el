;;; EXWM development
(push "~/dev/exwm" load-path)
;; v0.9 - e58ac743f97a005bb737e234a1eb1d467ed84bde
(push "~/dev/xelb" load-path)

(defun lg-minibuf-focus-in ()
  (select-frame-set-input-focus (selected-frame) t))

(defun lg-minibuf-focus-out ()
  (exwm-input--update-focus))

(add-hook 'minibuffer-setup-hook 'lg-minibuf-focus-in)
(add-hook 'minibuffer-exit-hook 'lg-minibuf-focus-out)


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

(exwm-input-set-key (kbd "C-x b") 'ido-switch-buffer)
(exwm-input-set-key (kbd "H-x b") 'lg-ido-switch-app)
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
                 "~/bin/lupe" "-override_redirect" "-font" "10x20" "-noshape" "-nohud" "-mag" "3"
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

(exwm-input-set-key (kbd "H-c d") 'multitran)

;;;


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
