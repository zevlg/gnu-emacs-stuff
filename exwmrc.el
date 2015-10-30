;;; EXWM development
(push "~/dev/exwm" load-path)
(push "~/dev/xelb" load-path)

(require 'exwm)
(require 'exwm-wconf)
(exwm-wconf-push)                       ;push initial
(exwm-wconf-tabs-mode 1)

(setq exwm-manage-switch-on-maprequest
      #'(lambda (x)
          (exwm-wconf-pop-to-buffer (exwm--id->buffer x))))

(exwm-input-set-key (kbd "H-r") 'exwm-reset)
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
(exwm-input-set-key (kbd "H-t") 'exwm-wconf-transpose)
(exwm-input-set-key (kbd "H-<return>") 'exwm-wconf-push)
(exwm-input-set-key (kbd "H-<delete>") 'exwm-wconf-remove)
(exwm-input-set-key (kbd "H-<backspace>") 'exwm-wconf-remove)
(exwm-input-set-key (kbd "H-C-l") 'exwm-wconf-other)
(exwm-input-set-key (kbd "H-x H-C-l") 'exwm-wconf-restore-buffer)

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

;; (defun lg-ido-switch-app ()
;;   (interactive)
;;   (cl-flet ((buffer-list (&optional frame)
;;              (remove-if-not #'(lambda (b)
;;                                 (with-current-buffer b
;;                                   (and (eq major-mode 'exwm-mode)
;;                                        (or (null frame) (eq exwm--frame frame)))))
;;                             (buffer-list))))
;;     (ido-switch-buffer)))
;; TODO
(exwm-input-set-key (kbd "H-x b") 'lg-ido-switch-app)

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
  (start-process "" nil "xlock"))

(exwm-input-set-key (kbd "H-a X") 'lg-exwm-start-xterm-screen)
(exwm-input-set-key (kbd "H-a x") 'lg-exwm-start-xterm)
(exwm-input-set-key (kbd "H-a f") 'lg-exwm-start-firefox)
(exwm-input-set-key (kbd "H-a l") 'lg-exwm-start-xlock)

(defun lg-xwem-lupe-in ()
  "Start lupe."
  (interactive)
  (start-process "" nil
   "~/bin/lupe" "-override_redirect" "-font" "10x20" "-noshape" "-nohud" "-mag" "3" "-geometry" "520x110+200+700")
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

;; exwmrc.el ends here
