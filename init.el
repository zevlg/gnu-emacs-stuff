;; init.el --- Custom configuration for GNU Emacs.
;;
;; Copyright (C) 2015,2016 by Zajcev Evgeny
;;
;;;;
(setq custom-file (expand-file-name "~/.emacs.d/lisp/lg-custom.el"))
(load custom-file)

;; On new host do
;;   M-x package-refresh-contents RET
;;   M-x package-install-selected-packages RET
;; after loading init.el fo the first time
;;
(setq gc-cons-threshold (* 4 8388608))
(setq-default garbage-collection-messages t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(require 'package)
;; (add-to-list 'package-archives
;;              '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(add-to-list 'package-pinned-packages '(telega . "melpa-stable"))

(package-initialize)
(require 'use-package)
;(require 'quelpa-use-package)

;; NOTE: Not needed in emacs27
;(package-initialize)

(push "~/.emacs.d/modules" load-path)
(push "~/.emacs.d/lisp" load-path)
(push "~/.emacs.d/thirdpart" load-path)
;; for git and git-blame
(push "/usr/share/git-core/emacs" load-path)

;; Theme
(deftheme lg-xemacs-like "xemacs-like theme")

(custom-theme-set-faces
 'lg-xemacs-like

 '(default ((t (:background "gray80" :foreground "black"))))
 '(cursor ((t (:foregound "Red3"))))
 '(border ((t (:foregound "black"))))

 '(blue ((t (:foreground "blue"))))
 '(bold ((t (:bold t))))
 '(bold-italic ((t (:italic t :bold t))))
 '(border-glyph ((t (nil))))
 '(custom-button-face ((t (:bold t))))
 '(custom-changed-face ((t (:background "blue" :foreground "white"))))
 '(custom-documentation-face ((t (nil))))
 '(custom-face-tag-face ((t (:underline t))))
 '(custom-group-tag-face ((t (:underline t :foreground "blue"))))
 '(custom-group-tag-face-1 ((t (:underline t :foreground "red"))))
 '(custom-invalid-face ((t (:background "red" :foreground "yellow"))))
 '(custom-modified-face ((t (:background "blue" :foreground "white"))))
 '(custom-rogue-face ((t (:background "black" :foreground "pink"))))
 '(custom-saved-face ((t (:underline t))))
 '(custom-set-face ((t (:background "white" :foreground "blue"))))
 '(custom-state-face ((t (:foreground "dark green"))))
 '(custom-variable-button-face ((t (:underline t :bold t))))
 '(custom-variable-tag-face ((t (:underline t :foreground "blue"))))
 '(dired-face-boring ((t (:foreground "Gray65"))))
 '(dired-face-directory ((t (:bold t))))
 '(dired-face-executable ((t (:foreground "SeaGreen"))))
 '(dired-face-flagged ((t (:background "LightSlateGray"))))
 '(dired-face-marked ((t (:background "PaleVioletRed"))))
 '(dired-face-permissions ((t (:background "grey75" :foreground "black"))))
 '(dired-face-setuid ((t (:foreground "Red"))))
 '(dired-face-socket ((t (:foreground "magenta"))))
 '(dired-face-symlink ((t (:foreground "cyan"))))
 '(font-lock-builtin-face ((t (:foreground "red3"))))
 '(font-lock-comment-face ((t (:foreground "blue4"))))
 '(font-lock-constant-face ((t (:foreground "red3"))))
 '(font-lock-doc-string-face ((t (:foreground "green4"))))
 '(font-lock-function-name-face ((t (:foreground "brown4"))))
 '(font-lock-keyword-face ((t (:foreground "red4"))))
 '(font-lock-preprocessor-face ((t (:foreground "blue3"))))
 '(font-lock-reference-face ((t (:foreground "red3"))))
 '(font-lock-string-face ((t (:foreground "green4"))))
 '(font-lock-type-face ((t (:foreground "steelblue"))))
 '(font-lock-variable-name-face ((t (:foreground "magenta4"))))
 '(font-lock-warning-face ((t (:bold t :foreground "Red"))))
 '(green ((t (:foreground "green"))))
 '(gui-button-face ((t (:background "grey75" :foreground "black"))))
 '(gui-element ((t (:background "Gray80"))))
 '(highlight ((t (:background "darkseagreen2"))))
 '(info-node ((t (:italic t :bold t))))
 '(info-xref ((t (:bold t))))
 '(isearch ((t (:background "paleturquoise"))))
 '(italic ((t (:italic t))))
 '(left-margin ((t (nil))))
 '(list-mode-item-selected ((t (:background "gray68"))))
 '(modeline ((t (:background "Gray80"))))
 '(modeline-buffer-id ((t (:background "Gray80" :foreground "blue4"))))
 '(modeline-mousable ((t (:background "Gray80" :foreground "firebrick"))))
 '(modeline-mousable-minor-mode ((t (:background "Gray80" :foreground "green4"))))
 '(parenthesis ((t (:foreground "gray60"))))
 '(paren-blink-off ((t (:foreground "gray80"))))
 '(paren-match ((t (:background "darkseagreen2"))))
 '(paren-mismatch ((t (:background "DeepPink" :foreground "black"))))
 '(pointer ((t (nil))))
 '(primary-selection ((t (:background "gray65"))))
 '(red ((t (:foreground "red"))))
 '(region ((t (:background "gray65"))))
 '(right-margin ((t (nil))))
 '(secondary-selection ((t (:background "paleturquoise"))))
 '(text-cursor ((t (:background "Red3" :foreground "gray80"))))
 '(toolbar ((t (:background "Gray80"))))
 '(underline ((t (:underline t))))
 '(vertical-divider ((t (:background "Gray80"))))
 '(widget-button-face ((t (:bold t))))
 '(widget-button-pressed-face ((t (:foreground "red"))))
 '(widget-documentation-face ((t (:foreground "dark green"))))
 '(widget-field-face ((t (:background "gray85"))))
 '(widget-inactive-face ((t (:foreground "dim gray"))))
 '(yellow ((t (:foreground "yellow"))))
 '(zmacs-region ((t (:background "gray65"))))
 '(escape-glyph ((t (:weight bold :background "gold" :foreground "blue"
                             :box (:line-width -1 :color "black")))))

 '(yascroll:thumb-fringe ((t :foreground "gray70" :background "gray70")))

 ;; org-mode
 '(org-indent ((t (:background "gray78" :foreground "gray78"))))
 '(org-block ((t (:background "gray75" :family "FreeMono" :height 0.85))))
 '(org-block-begin-line ((t (:background "gray75"))))
 )

(enable-theme 'lg-xemacs-like)

;; See https://github.com/zevlg/RictyDiminishedL
(set-face-attribute 'default nil :family "RictyDiminishedL" :height 360)

;(set-face-attribute 'fixed-pitch nil :family "Victor Mono Light" :height 0.95)
(set-face-attribute 'fixed-pitch nil :family "FreeMono" :height 0.85)
(set-face-attribute 'fixed-pitch-serif nil :family "FreeMono" :height 0.85)

;; Dash is 2 columns width
(set-char-table-range char-width-table '(?— . ?—) 2)

(setq inhibit-splash-screen t)
(setq enable-recursive-minibuffers t)
(setq select-enable-primary t)
;; Avoir queries for local variables
(setq enable-local-variables nil)

(setq max-specpdl-size 6400)
(setq max-lisp-eval-depth 16000)

;;; Browse kill ring
;; git clone https://github.com/browse-kill-ring/browse-kill-ring.git
;; Installs `M-y' binding to run `browse-kill-ring' command.
(use-package browse-kill-ring
   :init
   (setq browse-kill-ring-show-preview nil
         browse-kill-ring-highlight-current-entry t
         browse-kill-ring-highlight-inserted-item t)
   :config
   (browse-kill-ring-default-keybindings))

(defun lg-org-clock-toggle ()
  "Call clock-in or clock-out according to the current clocking state."
  (interactive)
  (call-interactively (if (org-clocking-p) #'org-clock-out #'org-clock-in)))

(use-package org
  :init
  (setq org-startup-indented t)
  (setq org-indent-mode-turns-on-hiding-stars nil)
  ;; Show links in open format
  ; (setq org-link-descriptive nil)
;  (setq org-adapt-indentation t)

  (setq org-catch-invisible-edits t)
  (setq org-log-done 'time)
  (setq org-log-redeadline 'time)

  (defun lg-org-return ()
    "Open a link at point or insert newline."
    (interactive)
    (let ((faces (get-text-property (point) 'face)))
      (if (or (when (listp faces)
                (memq 'org-link faces))
              (eq 'org-link faces))
          (call-interactively 'org-open-at-point)
        (call-interactively 'org-return))))

  (defun lg-org-emphasize ()
    (interactive)
    (save-excursion
      (call-interactively #'org-emphasize))
    )
  (add-hook 'org-mode-hook 'auto-fill-mode)
  :bind (("C-c o a" . org-agenda)
         ("C-c o l" . org-store-link)
         ("C-c o y" . org-insert-last-stored-link)
         ("C-c o c" . org-capture)
         ("C-c o o" . org-open-at-point)
         ("C-c o e" . org-export-dispatch)
         ("C-c o r" . lg-org-emphasize)

         ("C-c o n" . org-next-link)
         ("C-c o p" . org-previous-link)
         ("C-c o TAB" . org-next-link)
         ("C-c o <backtab>" . org-previous-link)

         :map org-mode-map
         ("RET" . lg-org-return)
         ;; clocking
         ("C-c c c" . lg-org-clock-toggle)))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package helm
  :defer t
  :commands (helm-git-grep)
  :config
  (set-face-attribute 'helm-source-header nil :height 0.85)
  )

(use-package company
  :init
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.1)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

(use-package company-posframe
  :after company
  :init
  ;; no quickhelp popups please
  (setq company-posframe-quickhelp-delay nil)
  :config
  ;; Do not enable it by default
;  (company-posframe-mode 1)
  )

(use-package ibuffer
  :init
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide) " "
                (size 9 -1 :right) " "
                (mode 16 16 :left :elide) " "
                (vc-status 16 16 :left) " "
                vc-relative-file)))
  :config
  (use-package ibuffer-vc))

(use-package page-break-lines
  :init
  (setq page-break-lines-char ?\x2001)
  :config
  (set-face-attribute 'page-break-lines nil :strike-through t))

;(use-package moccur-edit)

;; (require 'browse-kill-ring)
;; (browse-kill-ring-default-keybindings)
;; (setq browse-kill-ring-show-preview nil
;;       browse-kill-ring-highlight-current-entry t
;;       browse-kill-ring-highlight-inserted-item t)

;; make emacs use the clipboard for cut
(setq select-enable-clipboard t)
(defun lg-selection-value ()
  (let ((select-enable-clipboard nil))
    (gui-selection-value)))
(setq interprogram-paste-function 'lg-selection-value)

;(setq apropos-do-all t)
(setq apropos-do-all nil)
(define-key global-map (kbd "C-h a") #'apropos)

(setq Man-notify-method 'pushy)

(set-default 'indent-tabs-mode nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(put 'set-goal-column 'disabled nil)

(setq default-scroll-bar-width 6)
(scroll-bar-mode -1)

;; Disable mouse clicks
(require 'disable-mouse)
(global-disable-mouse-mode)

(mouse-avoidance-mode 'none)
(blink-cursor-mode 0)
(set-cursor-color "red3")
(setq x-stretch-cursor t)

(tooltip-mode -1)
;; Default value for show-help-function=tooltip-show-help-non-mode
(setq show-help-function nil)

(setq dabbrev-ignored-buffer-names
      '(" *Message-Log*" "*Messages*" "*Buffer List*" "*Ibuffer*"))

(define-key global-map (kbd "C-<tab>") 'other-window)
(define-key global-map (kbd "C-x TAB") (kbd "C-u - 1 TAB"))
(define-key global-map (kbd "C-x C-o") 'other-window)

(defun switch-to-other-buffer (arg)
  "Switch to the previous buffer.  With a numeric arg, n, switch to the nth
most recent buffer.  With an arg of 0, buries the current buffer at the
bottom of the buffer stack."
  (interactive "p")
  (if (eq arg 0)
      (bury-buffer (current-buffer)))
  (switch-to-buffer
   (if (<= arg 1) (other-buffer (current-buffer))
     (nth (1+ arg) (buffer-list)))))

(define-key global-map (kbd "C-M-l") 'switch-to-other-buffer)
(define-key global-map (kbd "C-x C-l") 'switch-to-other-buffer)
(require 'comint)
(define-key comint-mode-map (kbd "C-M-l") nil)

(defun lg-exchange-point-and-mark (arg)
  "`exchange-point-and-mark' with inverse meaning for ARG."
  (interactive "P")
  (exchange-point-and-mark (not arg)))

(define-key global-map (kbd "C-x C-x") 'lg-exchange-point-and-mark)

;; C-M-h to mark S expressions
(use-package lg-mark-sexp
  :init
  (defun lg-sexp-region (&optional pos)
    "Return region for the sexp around the POS."
    (let ((pnt (save-excursion
                 (ignore-errors
                   (when pos
                     (goto-char pos))
                   (cons (progn (up-list -1 t t) (point))
                         (if (eq (char-after) ?\")
                             ;; Mark following string
                             (progn
                               (forward-sexp)
                               (point))
                           (or (scan-lists (point) 1 0)
                               (buffer-end 1))))))))
      (when (and pnt (> (cdr pnt) (car pnt)))
        pnt)))
  (defun lg-mark-sexp (&optional n start-pos)
    "Mark N expressions around the point."
    (interactive "p")
    (when (> n 0)
      (when-let ((sexp-pos (lg-sexp-region)))
        (goto-char (car sexp-pos))
        (set-mark (cdr sexp-pos))
        (lg-mark-sexp (- n 1)))))

  :bind (("C-M-h" . lg-mark-sexp)
         ))

;(define-key term-mode-map (kbd "C-M-l") nil)
;(define-key py-shell-map (kbd "C-M-l") nil)

;;; icomplete to mimic `ido' for completions
;; RET forces first match
(use-package icomplete
  :init 
  (setq icomplete-compute-delay 0.15)
  (setq completion-show-help nil)
  (setq icomplete-show-matches-on-no-input t)
  (setq completion-styles '(basic partial-completion emacs22))

  (defun lg-icomplete-minibuffer-setup ()
    (when (and icomplete-mode (icomplete-simple-completing-p))
      (use-local-map (make-composed-keymap
                      icomplete-fido-mode-map (current-local-map)))
      ))
  (add-hook 'icomplete-minibuffer-setup-hook 'lg-icomplete-minibuffer-setup)
  :config
  (icomplete-mode 1)
  )

;;; NOTE: fido mode is way too slow on large collections
;; (ignore-errors
;;   (fido-mode 1))

;;; Use ido to switch buffers and open files
(use-package ido
  :init 
  (setq ido-max-window-height 1)
  ;; Use `.' for dired
  (setq ido-show-dot-for-dired t)
  ;; Disable confusing auto-merging
  (setq ido-auto-merge-work-directories-length -1)
  ;; Enter directory on `/'
  (setq ido-enter-matching-directory 'first)

  :config
  ;; Ignore telega chat buffers
  (add-to-list 'ido-ignore-buffers "\\`◀")
  (ido-mode 1))

;; M-x package-install RET undo-tree RET
;;; git clone http://www.dr-qubit.org/git/undo-tree.git
;;(push "~/.emacs.d/undo-tree" load-path)
(autoload 'undo-tree-undo "undo-tree" "Undo from redo package." t)
(autoload 'undo-tree-redo "undo-tree" "Redo from redo package." t)

(global-set-key (kbd "C-/") 'undo-tree-undo)
(global-set-key (kbd "C-_") 'undo-tree-undo)
(global-set-key (kbd "C-x C-/") 'undo-tree-redo)
(global-set-key (kbd "C-x C-_") 'undo-tree-redo)

(autoload 'goto-last-change "goto-last-change"
  "Set point to the position of the last change." t)
(global-set-key (kbd "C-c C-/") 'goto-last-change)
(global-set-key (kbd "C-c C-l") 'goto-last-change)

(global-undo-tree-mode)

;;; Use cool `ibuffer' instead of ugly `list-buffers'
(define-key global-map (kbd "C-x C-b") 'ibuffer)

(setq mouse-yank-at-point t)

(defun lg-mouse-yank ()
  "As `mouse-yank', but does not require to be bound to mouse."
  (interactive)
  (mouse-yank-at-click nil nil))

;; for slippery fingers pressing C-x C-c
(setq confirm-kill-emacs 'yes-or-no-p)

(defadvice save-buffers-kill-emacs (before lg-save-scratch-file activate)
  "Save scratch before exiting."
  (lg-save-lsf-buffer))

;;; Paren mode
(use-package paren
  :config
  (defun lg-show-paren-surround ()
    (let ((pdflt (show-paren--default)))
      (or pdflt
          (let ((pnt (save-excursion
                       (condition-case nil
                           (cons (progn (up-list -1) (point))
                                 (or (scan-lists (point) 1 0)
                                     (buffer-end 1)))
                         (error nil)))))
            (when (and pnt (> (cdr pnt) (car pnt)))
              (list (car pnt) (cdr pnt) nil nil nil))))))

  (setq show-paren-data-function #'lg-show-paren-surround)

  ;; NOTE: gray83 looks good on tty and X (with background=gray80)
  (set-face-background 'show-paren-match "gray83")
  (set-face-background 'show-paren-mismatch "#FFFF00")
  (set-face-foreground 'show-paren-mismatch nil)

  (setq show-paren-delay 0)
  (setq show-paren-style 'expression)
                                        ;(setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren nil)
  (setq show-paren-priority -1000)

  (show-paren-mode -1)
;  :hook (after-init-hook . show-paren-mode)
  )

;;; Selected region
;(set-face-background 'default "gray80")
(set-face-background 'region "gray65")

(setq fill-column 80)
(setq emacs-lisp-docstring-fill-column 70)

(setq scroll-error-top-bottom t)

;; Kill \n also if killing from the begining of line
(setq kill-whole-line t)
(setq kill-ring-max 50)

;; NOTE: With negative prefix arg `kill-line' will kill lines backward!
(defun lg-kill-line (&optional arg)
  "Deletes to the end of current line.
If ARG is given `lg-kill-line' deletes to the beginning of line."
  (interactive "P")
  (if (and arg (listp arg))
      (kill-region (point-at-bol) (point))
    (kill-line arg)))

(defun lg-kill-region (beg end &optional arg)
  "Delete or kill region at BEG END according to ARG.
If ARG is non-nil delete region, otherwise kill."
  (interactive "*r\nP")
  (if arg
      (delete-region beg end)
    (kill-region beg end)))

(defun lg-kill-current-buffer (buffer)
  "Kill current buffer without confirmation."
  (interactive (list (current-buffer)))
  (kill-buffer buffer))

(defun lg-kill-buffer-and-window (&rest args)
  "Kill current buffer and selected window withot confirmation."
  (interactive)
  (let ((buffer (current-buffer)))
    (delete-window (selected-window))
    (kill-buffer buffer)))

(defun lg-kill-other-buffer-and-window (arg)
  "Kill buffer in other window.
If prefix ARG is supplied, move point to other window."
  (interactive "P")

  (when (> (count-windows) 1)
    (other-window 1)
    (kill-buffer (current-buffer))
    (delete-window)

    (unless arg
      ;; Switch back
      (other-window -1))))

(define-key global-map (kbd "C-k") 'lg-kill-line)
(define-key global-map (kbd "C-w") 'lg-kill-region)

;; Killing bindings
(define-key global-map (kbd "M-<f4>") 'lg-kill-current-buffer)
(define-key global-map (kbd "C-x k") 'lg-kill-current-buffer)

(define-key global-map (kbd "C-M-k") 'kill-sexp)

;; backward kill sexp
(define-key global-map (kbd "C-M-<backspace>") (kbd "C-M-p C-M-k"))
(define-key global-map (kbd "C-x M-<backspace>") (kbd "C-M-p C-M-k"))

;;{{{ `-- Whitespace

(require 'whitespace)
(set-face-background 'whitespace-tab "yellow")
(set-face-background 'whitespace-space "LightBlue1")
(set-face-background 'whitespace-indentation "skyblue")

;;}}}

;;{{{ `-- Multitran

;;; Interface to multitran.com
;;
;; https://github.com/zevlg/multitran.el/
;; https://raw.githubusercontent.com/zevlg/emacs-stuff/master/wordfreq.el
(push "~/github/multitran.el" load-path)
(push "~/github/emacs-stuff" load-path)

(autoload 'multitran "multitran" nil t)
(autoload 'wordfreq-find "wordfreq" nil t)

(defun lg-multitran--hf-wordfreq ()
  "Show word's frequency rank."
  (let ((wfreq (wordfreq-find (or multitran-word ""))))
    (and wfreq (format "FRank: %S" (cadr wfreq)))))

(setq multitran-header-formatters
      '(miltitran--hf-word multitran--hf-languages
                           lg-multitran--hf-wordfreq multitran--hf-history))

(defun lg-multitran-customize ()
  (face-remap-add-relative
   'header-line `((:height ,(face-attribute 'default :height)) header-line)))

(add-hook 'multitran-mode-hook 'lg-multitran-customize)

(define-key global-map (kbd "C-c d d") 'multitran)
(define-key global-map (kbd "C-c d r") 'multitran)

;;}}}

;;{{{ `-- Google translate

(require 'google-translate)

(setq google-translate-default-source-language "auto"
      google-translate-default-target-language nil)

(defvar lg-google-translate-history nil "History for `read-string'.")

(defun lg-google-translate (text &optional override-p)
  "Translate TEXT using google translate.
Use `C-u' prefix to select languages."
  (interactive
   (let ((default-value (if (current-word)
                            (list (current-word))
                          lg-google-translate-history)))
     (list (if (use-region-p)
               (prog1
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (deactivate-mark))
             (read-string
              (cond (default-value
                      (format "Translate text [%s]: " (car default-value)))
                    (t "Translate text: "))
              nil 'lg-google-translate-history default-value))
           current-prefix-arg)))

  (let ((langs (google-translate-read-args override-p nil)))
    (google-translate-translate
     (car langs) (cadr langs) text))

  (pop-to-buffer "*Google Translate*")
  (local-set-key (kbd "w") 'lg-google-translate)
  (forward-button 1))

(defun lg-google-translate-inplace (start end)
  "Inplace translator.
If region is selected, then translate region.
If no region, translate word at point."
  (interactive (cond ((region-active-p)
                      (list (region-beginning) (region-end)))
                     ((bounds-of-thing-at-point 'word)
                      (let ((word-bounds (bounds-of-thing-at-point 'word)))
                        (list (car word-bounds) (cdr word-bounds))))
                     (t (user-error "Nothing to translate inplace."))))

  (let* ((reg-text (buffer-substring-no-properties start end))
         (langs (google-translate-read-args nil nil))
         (trans-text (google-translate-json-translation
                      (google-translate-request "auto" (cadr langs) reg-text))))
    (if (string-empty-p trans-text)
        (user-error "No translation for: %s" reg-text)

      (let ((saved-position (copy-marker (point) t)))
        (goto-char end)
        (insert trans-text)
        (goto-char saved-position)
        (delete-region start end)
        (message "Translated: %s" reg-text)))))

;;}}}

;;{{{ `-- Different bindings

;;; Remove annoying bindings
(define-key global-map (kbd "C-z") 'nil)
(define-key global-map (kbd "C-x C-=") 'nil)

(define-key global-map (kbd "C-=") 'what-cursor-position)
(define-key global-map (kbd "M-=") 'count-words)

;;}}}

;;{{{ `-- Useful interactive functions

;;; Some useful functions
(defun lg-maybeinsert (str &optional arg)
  "As identity, but inserts to selected buffer if ARG is non-nil."
  (if arg (insert str) str))

(defun alphabet (&optional arg)
  "Return latin alphabet as string or insert to selected buffer."
  (interactive "P")
  (lg-maybeinsert "abcdefghijklmnopqrstuvwxyz" arg))

(defun digits (&optional arg)
  "Return digits list as string or insert to selected buffer."
  (interactive "P")
  (lg-maybeinsert "1234567890" arg))

(defun true (&rest args)
  "Always return `t'."
  t)

(defun false (&rest args)
  "Always return `nil'."
  nil)

(defun debug-on-error (arg)
  "Toggle debug on error.
With negative ARG turn it off, with positive turn it on.
Otherwise toggle."
  (interactive "P")
  (setq debug-on-error
        (or (and (null arg)
                 (not debug-on-error))
            (and (not (null arg))
                 (> (prefix-numeric-value arg) 0))))
  (message "Debug on error is %s" (if debug-on-error "ON" "OFF")))

(defun debug-on-quit (arg)
  "Toggle debug on quit.
With negative ARG turn it off, with positive turn it on.
Otherwise toggle."
  (interactive "P")
  (setq debug-on-quit
        (or (and (null arg)
                 (not debug-on-quit))
            (and (not (null arg))
                 (> (prefix-numeric-value arg) 0))))
  (message "Debug on quit is %s" (if debug-on-quit "ON" "OFF")))

(defvar lg-scratch-file (expand-file-name "~/.emacs.d/*scratch-file*"))

(defun lg-switch-to-scratch (&optional arg)
  "Switch to \\*scratch\\* buffer.
If prefix ARG is specified, switch in other window."
  (interactive "P")
  (funcall (if arg 'switch-to-buffer-other-window 'switch-to-buffer)
           (let ((coding-system-for-read 'utf-8))
             (find-file-noselect lg-scratch-file))))

(defun lg-switch-to-init-el (&optional arg)
  (interactive "P")
  (funcall (if arg 'switch-to-buffer-other-window 'switch-to-buffer)
           (find-file-noselect (expand-file-name "~/.emacs.d/init.el"))))

(define-key global-map (kbd "M-<f3>") 'lg-switch-to-scratch)
(define-key global-map (kbd "C-<f3>") 'lg-switch-to-scratch)
(define-key global-map (kbd "C-c s") 'lg-switch-to-scratch)
(define-key global-map (kbd "C-c i") 'lg-switch-to-init-el)

(setq initial-major-mode 'lisp-interaction-mode)
(push '("\\*scratch-file\\*$" . lisp-interaction-mode) auto-mode-alist)

(defun lg-save-lsf-buffer ()
  "Save *scratch-file* buffer on exit."
  (when (buffer-live-p lsf-buffer)
    (with-current-buffer lsf-buffer
      (save-buffer))))

(electric-indent-mode -1)

(defun lg-insert-nl-at-eol (arg)
  "Insert new line at the end of line.
If prefix ARG is supplied, insert newline at point."
  (interactive "P")
  (unless arg
    (end-of-line))
  (newline-and-indent))

(define-key global-map (kbd "C-j") 'lg-insert-nl-at-eol)

;; To join two lines (aka vi's J)
(define-key global-map (kbd "C-^") (kbd "C-u M-^"))

(define-key global-map (kbd "C-l") 'recenter)

;;; For quick region commenting
(define-key global-map (kbd "C-c C-c") 'comment-region)
(define-key global-map (kbd "C-c ;") 'comment-region)

(autoload 'sh-backslash-region "sh-script" "" t)
(defun lg-backslash-region ()
  "Backslash current region"
  (interactive)
  (let ((tab-width 1))
    (call-interactively 'sh-backslash-region)))

(define-key global-map (kbd "C-c C-\\") 'lg-backslash-region)

(defun lg-activate-region ()
  "Activate previously active region."
  (interactive)
  (activate-mark))
(define-key global-map (kbd "C-M-z") 'lg-activate-region)

(defun lg-mark-eof ()
  (interactive)
  (set-mark (point-max)))
(defun lg-mark-bof ()
  (interactive)
  (set-mark (point-min)))
(define-key global-map (kbd "C->") 'lg-mark-eof)
(define-key global-map (kbd "C-<") 'lg-mark-bof)

(defvar netcat-history nil)

(defun netcat (addr)
  "Interactively connect to remote ADDR.
ADDR is string in form [<HOST>:]<PORT>"
  (interactive
   (list (read-string "Remote [<host>:]<port>: " nil 'netcat-history)))
  (let ((paddr (split-string addr ":"))
        (remote '("0" . "0")))
    (if (cadr paddr)
        (progn
          (setcar remote (car paddr))
          (setcdr remote (cadr paddr)))
      (setcdr remote (car paddr)))

    (switch-to-buffer (make-comint (concat "netcat-" addr) remote))
    (run-hooks 'netcat-hook)))

(defun lg-fixup-whitespace (arg)
  "Without prefix ARG run `delete-horizontal-space'.
With prefix ARG run `just-one-space'."
  (interactive "*P")
  (save-excursion
    (delete-horizontal-space)
    (when arg
      (insert ?\s))))

(define-key global-map (kbd "M-\\") 'lg-fixup-whitespace)
(define-key global-map (kbd "C-c SPC") 'just-one-space)

;; Do M-x lg-try-luck RET before serious work
(defun lg-try-luck (&optional luck-arg)
  "Try your luck.
Prefix arg LUCK-ARG specifies luck parameter, default is 4."
  (interactive (list (prefix-numeric-value (or prefix-arg 4))))
  (if (= (truncate (* luck-arg (/ (random most-positive-fixnum)
                                  (float most-positive-fixnum))))
         (1- luck-arg))
      (progn (message "No luck for today :(") nil)
    (progn (message "You are lucky!") t)))

;;}}}

;;{{{ `-- Mini calculator
;;; Useful mini calculator
(autoload 'calc-radix "calc-bin")
(autoload 'calc-hex-radix "calc-bin")
(autoload 'calc-decimal-radix "calc-bin")
(autoload 'calc-bin-radix "calc-bin")
(autoload 'calc-octal-radix "calc-bin")

(defun lg-mini-calc (expr &optional arg)
  "Calculate expression EXPR.
If ARG is given, then insert the result to current-buffer"
  (interactive
   (list (read-from-minibuffer "Calc expression: ")
         current-prefix-arg))
  (let ((result (calc-eval expr)))
    (if arg
        (insert result)
      (message "Result: [%s] = %s" expr result))))

(defun lg-calc-register (r cstr)
  "Recalculate value for register R.
CSTR specifies format string for `calc-eval' to be evaluated.
CSTR can contain special escape sequences:
 ~_  - for value of the current register
 ~n  - for value of the register n"
  (interactive "cCalc for register: \nsCalc string: ")
  (let* ((ss (split-string cstr "~"))
         (es (concat (car ss)
                     (mapconcat #'(lambda (s)
                                    (let* ((c (aref s 0))
                                           (cv (get-register (if (= c ?_) r c)))
                                           (os (substring s 1)))
                                      (format "%s %s" cv os)))
                                (cdr ss) ""))))
    (set-register r (string-to-int (calc-eval es)))
    (message "Register %c set to: %s" r (get-register r))))

;;}}}

;;{{{ `-- Scrolling

(setq scroll-step 1)
(setq scroll-preserve-screen-position 'always)

(setq next-screen-context-lines 0)

(define-key global-map (kbd "M-p") 'scroll-down-command)
(define-key global-map (kbd "M-n") 'scroll-up-command)

;;}}}

;;{{{ `-- Minibuffer

(setq echo-keystrokes 0.1)
(setq max-mini-window-height 0.5)
(setq resize-mini-windows nil)
(setq resize-mini-windows t)

;;}}}

;;{{{ `-- Modeline
(line-number-mode 1)
(column-number-mode 1)

(size-indication-mode 0)                ;file size
;(toggle-truncate-lines 1)
(setq truncate-partial-width-windows t)
(setq line-move-visual nil)

(setq display-time-default-load-average nil)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time-mode 1)

(setq line-number-mode t)
(setq column-number-mode t)

(set-face-background 'mode-line "#c0bf8d")
(set-face-attribute 'mode-line nil :height 120)

;;}}}

;;{{{ `-- Server

(ignore-errors
  (server-start))

;; Bold-ify/red-ify "Server"
(let ((sspec (assq 'server-buffer-clients minor-mode-alist))
      (server-string (propertize " Server" 'face 'error)))
  (if sspec
      (setcdr sspec (list server-string))
    (push '(server-buffer-clients server-string) minor-mode-alist)))

;;}}}

;;{{{ `-- Grep

;; Do not ask about saving some files
(setq grep-save-buffers nil)

(setq grep-use-directories-skip nil)

;; Use previous search regex if default tag is not found
(defun lg-find-tag-default ()
  "Use previous grep as default tag."
  (let ((f-t-d-f (get major-mode 'find-tag-default-function)))
  (or (funcall (or f-t-d-f 'find-tag-default))
      (car grep-regexp-history))))

(setq find-tag-default-function 'lg-find-tag-default)

(defun lg-grep-glob ()
  "Calculate glob pattern according to current buffer filename."
  (let* ((bn (or (buffer-file-name)
                 (replace-regexp-in-string "<[0-9]+>\\'" "" (buffer-name))))
         (fn (and bn
                  (stringp bn)
                  (file-name-nondirectory bn)))
         (ext (and fn
                   (let ((ext (file-name-extension fn)))
                     (and ext (concat "*." ext))))))
    (or ext "*")))

(defun lg-grep (regexp &optional files-glob template)
  "*Run grep to match REGEXP in FILES."
  (interactive
   (progn
     (grep-compute-defaults)
     (let* ((regexp (grep-read-regexp))
            (files-glob (or (and current-prefix-arg (grep-read-files regexp))
                            (lg-grep-glob)))
            (template grep-template))
       (when (string-suffix-p ".gz" files-glob)
         (let ((grep-program "zgrep")
               ;; Don't change global values for variables computed
               ;; by `grep-compute-defaults'.
               (grep-template nil)
               (grep-command nil)
               (grep-host-defaults-alist nil)
               (grep-use-null-filename-separator nil))
           ;; Recompute defaults using let-bound values above.
           (grep-compute-defaults)
           (setq template grep-template)))

       (list regexp files-glob template))))

  (let ((grep-template template)
        (grep-find-ignored-files nil))
    (lgrep regexp files-glob default-directory nil)))

(defun lg-copy-buffer-name ()
  "Copy current buffer filename into kill-ring."
  (interactive)
  (kill-new (buffer-file-name))
  (message "Copied: %s" (buffer-file-name)))

;;}}}

;;{{{ `-- Git

(autoload 'git-status "git" "git-status" t)

(defun lg-git-status-install-keys ()
  (local-set-key (kbd "<tab>") 'git-next-file)
  (local-set-key (kbd "<backtab>") 'git-prev-file))

(add-hook 'git-status-mode-hook 'lg-git-status-install-keys)

;; Make vc-push/vc-update work in git-status buffer
(defun lg-git-status-vc-deduce-fileset (orig-fun &rest args)
  (if (eq major-mode 'git-status-mode)
      `(Git (,default-directory))
    (apply orig-fun args)))

(advice-add 'vc-deduce-fileset :around #'lg-git-status-vc-deduce-fileset)

;; Paste to gist
(require 'gist)

(defvar lg-gist-description nil
  "Description for the gist currently importing.
Bind it to change the description.")

;; Override it to take first few chars of the gist as description
(defun gist-ask-for-description-maybe ()
  lg-gist-description)

(defun lg-gist-region (begin end &optional arg)
  "Paste region to gist.
If prefix ARG is specified - create public gist."
  (interactive "r\nP")

  (let ((lg-gist-description
         (replace-regexp-in-string
          "\n" " " (buffer-substring
                    begin (if (> (- end begin) 30) (+ begin 30) end)))))
    (gist-region begin end (not arg)))

  ;; GNU Emacs keeps region active after evaluation, so force
  ;; deactivation
  (deactivate-mark))

;; C-x C-s will notify when buffer is written
;; (advice-add 'gist-mode-save-buffer :after
;;             (lambda ()
;;               "Notify when buffer is written."
;;               (message "Wrote %s" (buffer-name))))


(use-package gist-org
  :load-path "~/github/gist-org.el"
  :init
  (defun lg-gist-open-index ()
    "Open gist with my index.org file."
    (interactive)
    (gist-org-open lg-gist-index-id)) 

  :config
  (gist-org-mode-setup)
 )


(defun lg-buffer-file-git-p (&optional buffer)
  "Return non-nil if BUFFER's file is tracked in git."
  (vc-git-responsible-p (or (buffer-file-name buffer) default-directory)))

(defun lg-grep-maybe-git ()
  "If file is tracked by git, then use `helm-git-grep'.
Otherwise drop to `lg-grep'."
  (interactive)
  (if (lg-buffer-file-git-p)
      (helm-git-grep-1 (grep-tag-default))
;      (call-interactively 'helm-git-grep)
    (call-interactively 'lg-grep)))

(add-hook 'gist-list-mode-hook 'highline-local-on)

;;}}}

;;{{{   `-- C-o - Prefix for -other- commands

;; Make C-o to be keymap for othering
(define-key global-map (kbd "C-O") 'open-line)

(define-key global-map (kbd "C-o") nil)
(define-key global-map (kbd "C-o 0") 'lg-kill-buffer-and-window)
(define-key global-map (kbd "C-o k") 'lg-kill-other-buffer-and-window)
(define-key global-map (kbd "C-o C-f") 'find-file-other-window)
(define-key global-map (kbd "C-o v") 'find-variable-other-window)
(define-key global-map (kbd "C-o f") 'find-function-other-window)
(define-key global-map (kbd "C-o l") 'find-library-other-window)
(define-key global-map (kbd "C-o t") 'find-tag-other-window)
(define-key global-map (kbd "C-o b") 'ido-switch-buffer-other-window)
(define-key global-map (kbd "C-o a") 'add-change-log-entry-other-window)
(define-key global-map (kbd "C-o d") 'dired-other-window)
(define-key global-map (kbd "C-o C-o") 'iswitchb-display-buffer)
(define-key global-map (kbd "C-o M-C-l") 'lg-switch-to-other-other-window)

;;}}}

;;{{{ `-- C-cf - Prefix for Finding commands

(autoload 'copy-from-above-command "misc" "Copy line from the above" t)

;; C-cf Prefix for FIND functions
(define-key global-map (kbd "C-c f f") 'find-function)
(define-key global-map (kbd "C-c f F") 'find-face-definition)
(define-key global-map (kbd "C-c f v") 'find-variable)
(define-key global-map (kbd "C-c f l") 'find-library)
(define-key global-map (kbd "C-c f k") 'find-function-on-key) ;; equivalent to C-x K
(define-key global-map (kbd "C-c f t") 'find-tag)
(define-key global-map (kbd "C-c f T") 'lg-find-tag-regex)
(define-key global-map (kbd "C-c f h") 'hexl-find-file)
(define-key global-map (kbd "C-c f a") 'copy-from-above-command)

;;}}}

;;{{{ `-- Pair skeleton
;; do not insert newline after skeleton insertation
(setq skeleton-end-newline nil)

(defvar lg-skeleton-pairs
  '((?\" . (?\" ?\" ?\" _ ?\"))
    (?\( . (?\( ?\) ?\( _ ?\)))
    (?\[ . (?\[ ?\] ?\[ _ ?\]))
    (?\{ . (?\{ ?\} ?\{ _ ?\}))
    (?\' . (?\' ?\' ?\' _ ?\'))
    (?\` . (?\` ?\' ?\` _ ?\'))
    (?<  . (?< ?> ?< _ ?>)))
  "Table of skeletons pairs. Maybe local to buffer.")

(defvar lg-skeleton-pair-char-syntaxes
  '((?_ . ?w)
    (?- . ?w))
  "Character syntaxes to modify when inserting pairs.")

(defun lg-skeleton-pair-insert (arg)
  "Inserts pairs."
  (interactive "*P")

  (let* ((chr (event-basic-type last-command-event))
         (pair (assoc chr lg-skeleton-pairs)))
    (if (null pair)
        (message "Character `%c' is not in `lg-skeleton-pairs'." chr)
      (cond ((and (listp arg) (not (null arg)))
             ;; Surround current word with
             (let ((sse (mapcar (lambda (c)
                                  (cons c (char-syntax c)))
                                (mapcar 'car lg-skeleton-pair-char-syntaxes))))
               ;; Modify character syntax table entries
               (mapc (lambda (cse)
                       (modify-syntax-entry
                        (car cse) (char-to-string (cdr cse))))
                     lg-skeleton-pair-char-syntaxes)

               (save-excursion
                 (unless (looking-at "\\<")
                   (backward-word 1))
                 (when (looking-at "\\sw")
                   (let ((pl 0)
                         (r (prefix-numeric-value arg)))
                     (while (> r 1)
                       (setq r (/ r 4))
                       (setq pl (1+ pl)))

                     (insert (make-string pl (nth 0 (cdr pair))))
                     (forward-word 1)
                     (insert (make-string pl (nth 1 (cdr pair)))))))

               ;; Restore character syntax table entries
               (mapc (lambda (cse)
                       (modify-syntax-entry
                        (car cse) (char-to-string (cdr cse))))
                     sse)))

            (t (dotimes (not-used (prefix-numeric-value arg))
                 ;; XXX hack to make M-( work in slime's REPL
                 (cl-flet ((insert-before-markers (&rest args)
                                               (apply #'insert args)))
                   (skeleton-insert
                    (cons nil (cdddr pair))))))))))

(define-key global-map (kbd "C-M-{") 'backward-paragraph)
(define-key global-map (kbd "C-M-}") 'forward-paragraph)
(define-key global-map (kbd "M-]") 'forward-paragraph)
(define-key global-map (kbd "M-}") 'backward-paragraph)
(define-key global-map (kbd "C-M-'") 'abbrev-prefix-mark)

(define-key global-map (kbd "M-\"") 'lg-skeleton-pair-insert)
(define-key global-map (kbd "M-`") 'lg-skeleton-pair-insert)
(define-key global-map (kbd "M-'") 'lg-skeleton-pair-insert)
(define-key global-map (kbd "M-{") 'lg-skeleton-pair-insert)
(define-key global-map (kbd "M-(") 'lg-skeleton-pair-insert)
(define-key global-map (kbd "M-[") 'lg-skeleton-pair-insert)

;; pairing for C-mode
(defun lg-cmode-install-skeletor-pairs ()
  "C-mode specific skeleton pair for curly braces.
M-{ causes next skeleton insertation.
{
        X
}"
  ;; make a copy so `setcdr' won't affect initial value
  (set (make-local-variable 'lg-skeleton-pairs)
       (copy-alist lg-skeleton-pairs))
  (setcdr (assq ?\{ lg-skeleton-pairs)
          '(?{ ?} ?{ '(progn (indent-according-to-mode) nil) \n _ \n ?}
               '(progn (indent-according-to-mode) nil))))

(add-hook 'c-mode-hook 'lg-cmode-install-skeletor-pairs)
(add-hook 'objc-mode-hook 'lg-cmode-install-skeletor-pairs)

;;}}}

;;{{{ `-- Highlight current line

(defun highline-local-on ()
  "Ensure current line is highligted."
  (interactive)
  (hl-line-mode 1))

;;; Use highline in several major modes by default
(add-hook 'ibuffer-hook 'highline-local-on)
(add-hook 'cvs-mode-hook 'highline-local-on)

;;}}}

;;{{{ `-- ELDOC customization

;; Fast refresh
(setq eldoc-idle-delay 0.1)

;;}}}

;;{{{ `-- Dot-mode
;; https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/dot-mode.el
(require 'dot-mode)
(add-hook 'find-file-hooks 'dot-mode-on)

(defun lg-dot-mode-apply-to-region-lines (begin end)
  "Similiar to `apply-macro-to-region-lines', but for `dot-mode-execute'."
  (interactive "r")
  (if (null dot-mode-cmd-buffer)
      (message "Nothing to repeat")

    ;; Don't want execution to kick off infinite recursion
    (remove-hook 'pre-command-hook 'dot-mode-pre-hook t)
    (remove-hook 'post-command-hook 'dot-mode-loop t)
    (remove-hook 'after-change-functions 'dot-mode-after-change t)

    (apply-macro-to-region-lines begin end dot-mode-cmd-buffer)

    ;; Put the hooks back
    ;; (make-local-hook 'pre-command-hook)
    ;; (make-local-hook 'post-command-hook)
    ;; (make-local-hook 'after-change-functions)
    (add-hook 'pre-command-hook 'dot-mode-pre-hook nil t)
    (add-hook 'post-command-hook 'dot-mode-loop nil t)
    (add-hook 'after-change-functions 'dot-mode-after-change nil t)))

(defun lg-dot-mode-execute (n)
  "Execute either `dot-mode-execute' or `lg-dot-mode-apply-to-region-lines'."
  (interactive "p")
  (if (region-active-p)
      (dotimes (i n)
        (lg-dot-mode-apply-to-region-lines
         (region-beginning) (region-end)))
    (dotimes (i n)
      (dot-mode-execute))))

(define-key dot-mode-map (kbd "C-.") 'lg-dot-mode-execute)

;; Show `dot-mode-cmd-buffer' in modeline

;;}}}

;;{{{ `-- Emacs lisp mode

;;; Dim parens
(use-package paren-face
  :ensure t
  :config
  (global-paren-face-mode 1))

(defun lg-emacs-lisp-mode-customize ()
  (company-mode 1)
  (local-set-key (kbd "C-c c c") 'byte-compile-file)
  )

(add-hook 'emacs-lisp-mode-hook 'which-function-mode)
(add-hook 'emacs-lisp-mode-hook 'lg-emacs-lisp-mode-customize)

;; Evaluate current-buffer
(defun lg-emacs-eval-buffer (buffer)
  "Evaluate BUFFER."
  (interactive (list (current-buffer)))
  (eval-buffer buffer)
  (message (format "Buffer(%s) evaluated." (buffer-name buffer))))

;; Evaluate active region
(defun lg-emacs-eval-region (start end &optional arg)
  "Evaluate region from START to END.
If prefix ARG is specified, then replace region with the evaluation result."
  (interactive "r\nP")
  (save-excursion
    (let ((res ""))
      (eval-region start end
                   (lambda (chr)
                     (setq res (concat res (char-to-string chr)))))
      ;; GNU Emacs keeps region active after evaluation, so force
      ;; deactivation
      (deactivate-mark)

      ;; Fixate RES
      (when (> (length res) 1)
        (setq res (substring res 1 (1- (length res)))))
      ;; Display or insert RES
      (if (null arg)
          (message "Region(%d %d) => %s" start end res)
        (delete-region start end)
        (insert res)))))

(defun lg-macroexpand-sexp ()
  "Expand emacs lisp macro at point."
  (interactive)
  (save-excursion
    (let ((sexp (read (current-buffer))))
      (unless (listp sexp)
        (backward-up-list)
        (setq sexp (read (current-buffer))))
      (pp-macroexpand-expression sexp))))

;; NOTE: Installs C-ce Prefix for elisp operations (for lisp-mode only)
(define-key emacs-lisp-mode-map (kbd "C-c e b") 'lg-emacs-eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c e r") 'lg-emacs-eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c e f") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c e s") 'eval-last-sexp)
;; NOTE: There is also `emacs-lisp-macroexpand'
(define-key emacs-lisp-mode-map (kbd "C-c e e") 'lg-macroexpand-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c e m") 'lg-macroexpand-sexp)

(define-key lisp-interaction-mode-map (kbd "C-c e b") 'lg-emacs-eval-buffer)
(define-key lisp-interaction-mode-map (kbd "C-c e r") 'lg-emacs-eval-region)
(define-key lisp-interaction-mode-map (kbd "C-c e f") 'eval-defun)
(define-key lisp-interaction-mode-map (kbd "C-c e s") 'eval-last-sexp)
(define-key lisp-interaction-mode-map (kbd "C-c e e") 'pp-macroexpand-last-sexp)

;;}}}

;;{{{ `-- Editing commands

(setq python-shell-interpreter "python3")
(setq py-shell-name "python3")
(setq python-shell-buffer-name "Python3")

;; shutup "Shell native completion is enabled."
(remove-hook 'python-shell-first-prompt-hook
             #'python-shell-completion-native-turn-on-maybe-with-msg)
(add-hook 'python-shell-first-prompt-hook
          #'python-shell-completion-native-turn-on-maybe)


(defun lg-py-shell ()
  "Switch to python interpreter."
  (interactive)
  (unless (python-shell-get-process)
    (run-python))
  (pop-to-buffer
   (process-buffer (python-shell-get-process-or-error))))

(define-key global-map (kbd "C-c d p") 'lg-py-shell)

;;; Python based mini calculator
(defun lg-python-mini-calc (pyexpr &optional arg)
  "Evaluate python expression.
If prefix ARG is given then insert result into the current buffer."
  (interactive
   (list (read-from-minibuffer "Python calc: ")
         current-prefix-arg))
  (unless (python-shell-get-process)
    (run-python)
    (accept-process-output (python-shell-get-process-or-error) 0.5))

  (let ((proc (python-shell-get-process-or-error)) res)
    (with-current-buffer (process-buffer proc)
      (save-excursion
        (goto-char (point-max))
        (insert pyexpr "  # from `lg-python-mini-calc'")
        (comint-send-input)
        (accept-process-output proc 1)

        (goto-char (process-mark proc))
        (forward-line 0)
        (setq res (substring (buffer-substring-no-properties
                              comint-last-input-end (point))
                             0 -1))))
    (if arg
        (insert res)
      (message "Result: [%s] = %s" pyexpr res))))
(global-set-key (kbd "M-#") 'lg-python-mini-calc)

;; C-ce Editing prefix
(define-key global-map (kbd "C-c e c") 'checkdoc)
(define-key global-map (kbd "C-c e w") 'whitespace-cleanup)
(define-key global-map (kbd "C-c e o") 'delete-blank-lines)
(define-key global-map (kbd "C-c e a") 'align)
;;(define-key global-map (kbd "C-c e l") 'lg-fix-long-lines)
;;(define-key global-map (kbd "C-c e i") 'lg-ispell-region-or-buffer)
;;(define-key global-map (kbd "C-c e y") 'yofy)

;; C-cr - Prefix for REGULAR EXPRESSIONS
(define-key global-map (kbd "C-c r b") 're-builder)
(define-key global-map (kbd "C-c r s") 're-search-forward)
(define-key global-map (kbd "C-c r r") 're-search-backward)
(define-key global-map (kbd "C-c r e") 'query-replace-regexp)
(define-key global-map (kbd "C-c r g") 'lg-grep)
(define-key global-map (kbd "C-c r o") 'occur)

;; C-cm Prefix for MISC commands
(define-key global-map (kbd "C-c m f") 'folding-mode)
(define-key global-map (kbd "C-c m a") 'ascii-display)
(define-key global-map (kbd "C-c m g") 'imenu)
(define-key global-map (kbd "C-c m o") 'occur)
(define-key global-map (kbd "C-c m |") 'vertical-mode)
(define-key global-map (kbd "C-c m v") 'fci-mode)
(define-key global-map (kbd "C-c m h") 'hl-line-mode)
(define-key global-map (kbd "C-c m w") 'whitespace-mode)
(define-key global-map (kbd "C-c m F") 'flyspell-mode)
(define-key global-map (kbd "C-c m i") 'flyspell-mode)
(define-key global-map (kbd "C-c m m") 'himarks-mode)
(define-key global-map (kbd "C-c m s") 'stripes-mode)
(define-key global-map (kbd "C-c m l") 'ruler-mode)
(define-key global-map (kbd "C-c m d") 'dot-mode)
(define-key global-map (kbd "C-c m r") 'ruler-mode)
(define-key global-map (kbd "C-c m k") 'keycast-mode) ;show last keys pressed
(define-key global-map (kbd "C-c m <RET>") 'hide-cr-mode)
(define-key global-map (kbd "C-c m p") 'pabbrev-mode)

;; C-cd Prefix for DICT
(push "~/github/grammarbot.el" load-path)
(autoload 'grammarbot "grammarbot" "Check grammar with grammarbot." t)

(defun lg-flyspell-region-or-buffer ()
  "Flyspell region or buffer."
  (interactive)
  (if (region-active-p)
      (progn
        (call-interactively 'flyspell-region)
        (deactivate-mark))
    (flyspell-buffer)))

(define-key global-map (kbd "C-c d c") 'lg-flyspell-region-or-buffer)
(define-key global-map (kbd "C-c d f") 'lg-flyspell-region-or-buffer)
(define-key global-map (kbd "C-c d d") 'multitran)
(define-key global-map (kbd "C-c d r") 'multitran)
(define-key global-map (kbd "C-c d t") 'lg-google-translate)
(define-key global-map (kbd "C-c d i") 'lg-google-translate-inplace)
(define-key global-map (kbd "C-c d g") 'grammarbot)
(define-key global-map (kbd "C-c d w") 'wolfram-alpha)

;;}}}

;;{{{   `-- C-cl - Prefix for Listing commands

(put 'list-threads 'disabled nil)
(put 'list-timers 'disabled nil)

(define-key global-map (kbd "C-c l a") 'list-abbrevs)
(define-key global-map (kbd "C-c l b") 'list-bookmarks)
(define-key global-map (kbd "C-c l h") 'list-command-history)
(define-key global-map (kbd "C-c l c") 'list-colors-display)
(define-key global-map (kbd "C-c l f") 'list-faces-display)
(define-key global-map (kbd "C-c l i") 'list-timers)
(define-key global-map (kbd "C-c l m") 'list-matching-lines)
(define-key global-map (kbd "C-c l t") 'describe-text-properties)
(define-key global-map (kbd "C-c l T") 'list-threads)
(define-key global-map (kbd "C-c l p") 'list-processes)
(define-key global-map (kbd "C-c l s") 'strokes-list-strokes)
(define-key global-map (kbd "C-c l .") 'list-tags)
(define-key global-map (kbd "C-c l k") 'browse-kill-ring)
(define-key global-map (kbd "C-c l r") 'list-registers)
(define-key global-map (kbd "C-c l o") 'list-packages)
(define-key global-map (kbd "C-c l w") 'whitespace-report)
(define-key global-map (kbd "C-c l g") 'gist-list)

;;}}}

;;{{{   `-- C-cc - Prefix for Count/Compile commands

(define-key global-map (kbd "C-c c w") 'count-words)
(define-key global-map (kbd "C-c c m") 'count-matches)

(define-key global-map (kbd "C-c c t") 'lg-compile-ctest-target)
(define-key global-map (kbd "C-c c b") 'lg-copy-buffer-name)

;; Also prefix for compilation
(define-key global-map (kbd "C-c c C") 'compile)

;;}}}

;;{{{   `-- C-cg - Git commands

(define-key global-map (kbd "C-x v p") 'vc-push)
(define-key global-map (kbd "C-x v S") 'vc-create-tag)
(define-key global-map (kbd "C-x v s") 'git-status)

(define-key global-map (kbd "C-c g a") 'lg-vc-annotate)
(define-key global-map (kbd "C-c g =") 'vc-diff)
(define-key global-map (kbd "C-c g p") 'vc-push)
(define-key global-map (kbd "C-c g u") 'vc-update)

(define-key global-map (kbd "C-c g r") 'lg-gist-region)
(define-key global-map (kbd "C-c g n") 'lg-gist-open-index)
(define-key global-map (kbd "C-c g i") 'lg-gist-open-index)
(define-key global-map (kbd "C-c g g") 'lg-gist-open-index)
(define-key global-map (kbd "C-c g b") 'gist-org-buffer-switch)
(define-key global-map (kbd "C-c g +") 'gist-org-file-create)
(define-key global-map (kbd "C-c g l") 'gist-list)

;;; Magit
;; This requires ido-completing-read+
;(setq magit-completing-read-function 'magit-ido-completing-read)

(setq magit-diff-highlight-hunk-region-functions
      '(magit-diff-highlight-hunk-region-using-overlays
        magit-diff-highlight-hunk-region-using-underline))

(setq magit-uniquify-buffer-names nil)
(setq magit-buffer-name-format "%x%M%v: %t%x")

(define-key global-map (kbd "C-c g s") 'magit-status)

(defun lg-magit-display-func (buffer)
  (let ((buffer-major-mode (with-current-buffer buffer major-mode)))
    ;; NOTE: status and diff from status display in same window
    (if (or (eq buffer-major-mode 'magit-status-mode)
            (and (eq major-mode 'magit-status-mode)
                 (eq buffer-major-mode 'magit-diff-mode)))
        (display-buffer buffer '(display-buffer-same-window))

      (magit-display-buffer-traditional buffer))))

(defun lg-magit-diff-dwim (&optional args files)
  "If has file at point, execute diff only for this file."
  (interactive (magit-diff-arguments))
  (if-let ((file (magit-file-at-point)))
      (magit-diff-dwim args (list file))
    (magit-diff-dwim args files)))

(defun lg-magit-status-install-keys ()
  ;; Do not open sections if searching with isearch
  (setq-local isearch-filter-predicate #'isearch-filter-visible)

  (define-key magit-status-mode-map (kbd "=") 'lg-magit-diff-dwim)
  )

(add-hook 'magit-status-mode-hook 'lg-magit-status-install-keys)

(setq magit-display-buffer-function 'lg-magit-display-func)
(setq magit-section-initial-visibility-alist '((untracked . hide)))

;;}}}

;;{{{ `-- Python

;; Builtint `python-mode' is slow in emacs 25, so use this one
;; https://gitlab.com/python-mode-devs/python-mode.git
(push "~/.emacs.d/lisp/python-mode" load-path)
(require 'python-mode)

;; make flymake marks more visible
(define-fringe-bitmap 'flymake-double-exclamation-mark
  (vector #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111))

(setq elpy-modules '(elpy-module-sane-defaults
                     elpy-module-company
                     elpy-module-eldoc
                     elpy-module-flymake
                     elpy-module-pyvenv))

;; See https://github.com/jorgenschaefer/elpy/issues/1381
(setq elpy-eldoc-show-current-function nil)

(elpy-enable)

(defun lg-py-install-keys ()
  (local-set-key (kbd "C-c e r") 'py-execute-region)
  (local-set-key (kbd "C-c e b") 'py-execute-buffer)
  (local-set-key (kbd "C-c e f") 'py-execute-def-or-class)
  (local-set-key (kbd "C-c e s") 'py-execute-string)
  (local-set-key (kbd "C-c C-c") 'py-comment-region)
  (local-set-key (kbd "C-j") 'lg-insert-nl-at-eol)
  )

(add-hook 'python-mode-hook 'lg-py-install-keys)

;;; Cython mode
(require 'cython-mode)

;; For scons
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))

;;}}}

;; Save places of visited files
(require 'saveplace)
(setq save-place-file (expand-file-name "places" user-emacs-directory))
(save-place-mode 1)

(defun emacs-places-save ()
  "Save places to file."
  (interactive)
  (save-place-kill-emacs-hook))

;;; Sudoku
;;
(autoload 'sudoku "sudoku" "Start playing sudoku." t)

(setq sudoku-level 'evil)
(setq sudoku-download nil)

;; Enable autoinserter
(setq sudoku-autoinsert-mode t)
(add-hook 'sudoku-after-change-hook 'sudoku-autoinsert)

;;; Version Control
(setq vc-follow-symlinks t)

(defvar lg-before-annotate-wconf nil
  "Window configuration before `vc-annotate'.")

(defun lg-vc-show-commit-msg ()
  (interactive)
  (save-selected-window
    (vc-annotate-show-log-revision-at-line)))

(defun lg-vc-annotate ()
  "Call `vc-annotate' saving window configuration."
  (interactive)
  (setq lg-before-annotate-wconf
        (current-window-configuration))
  (call-interactively 'vc-annotate))

(defun lg-vc-annotate-quit ()
  (interactive)
  (call-interactively 'quit-window)
  (when lg-before-annotate-wconf
    (set-window-configuration lg-before-annotate-wconf)
    (setq lg-before-annotate-wconf nil)))

;; Unfortunatelly message "Annotating... done" shadows our message :(
(defun lg-on-vc-annotate ()
  (local-set-key (kbd "q") 'lg-vc-annotate-quit)
  (local-set-key (kbd "l") 'lg-vc-show-commit-msg)

  (make-local-variable 'post-command-hook)
  (add-hook 'post-command-hook 'lg-vc-show-commit-msg))

(add-hook 'vc-annotate-mode-hook 'lg-on-vc-annotate)

(define-key global-map (kbd "C-x v g") 'lg-vc-annotate)

;;{{{ `--Calendar

(require 'calendar)
(defalias 'cal 'calendar)
(setq calendar-week-start-day 1)

(set-face-attribute 'calendar-today nil :weight 'bold)
(set-face-background 'holiday "DarkSeaGreen3")

(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(add-hook 'calendar-mode-hook 'calendar-mark-holidays)

(setq lg-russian-holdays
      '((holiday-fixed 1 1 "Новогодние каникулы")
        (holiday-fixed 1 2 "Новогодние каникулы")
        (holiday-fixed 1 3 "Новогодние каникулы")
        (holiday-fixed 1 4 "Новогодние каникулы")
        (holiday-fixed 1 5 "Новогодние каникулы")
        (holiday-fixed 1 6 "Новогодние каникулы")
        (holiday-fixed 1 7 "Рождество Христово")
        (holiday-fixed 1 8 "Новогодние каникулы")
        (holiday-fixed 2 23 "День защитника Отечества")
        (holiday-fixed 3 8 "Международный женский день")
        (holiday-fixed 5 1 "День Весны и Труда")
        (holiday-fixed 5 9 "День Победы")
        (holiday-fixed 6 12 "День России")
        (holiday-fixed 11 4 "День народного единства")))

(setq calendar-holidays lg-russian-holdays)

(defface lg-weekend-face nil nil)
(set-face-background 'lg-weekend-face "pink")

(defun lg-visible-weekends-list ()
  (let* ((m1 displayed-month)
         (y1 displayed-year)
         (m2 displayed-month)
         (y2 displayed-year)
         ;; Absolute date of first/last dates in calendar window.
         (start-date (progn
                       (calendar-increment-month m1 y1 -1)
                       (calendar-absolute-from-gregorian (list m1 1 y1))))
         (end-date (progn
                     (calendar-increment-month m2 y2 1)
                     (calendar-absolute-from-gregorian
                      (list m2 (calendar-last-day-of-month m2 y2) y2)))))

    (cl-remove-if-not #'(lambda (x)
                          (member (mod x 7) calendar-weekend-days))
                      (number-sequence start-date end-date))))
  ;; (let ((start-date (calendar-absolute-from-gregorian (list 1 1 displayed-year)))
  ;;       (end-date (calendar-absolute-from-gregorian (list 12 31 displayed-year))))
  ;;   (remove-if-not #'(lambda (x)
  ;;                      (member (mod x 7) calendar-weekend-days))
  ;;                  (number-sequence start-date end-date))))

(defun lg-mark-weekends (&optional face)
  "Mark all weekends with FACE"
  (dolist (weekend (lg-visible-weekends-list))
    (calendar-mark-visible-date
     (calendar-gregorian-from-absolute weekend)
     (or face 'lg-weekend-face))))

(add-hook 'calendar-today-visible-hook 'lg-mark-weekends)
(add-hook 'calendar-today-invisible-hook 'lg-mark-weekends)

;;}}}

;;{{{ `-- Autoinsert
(require 'autoinsert)
(add-hook 'find-file-hook 'auto-insert)

(push '(("\\.el\\'" . "Emacs Lisp header")
        "Short description: "
        ";;; " (file-name-nondirectory (buffer-file-name)) " --- " str "  -*- lexical-binding: t -*-

;; Copyright (C) "
        (substring (current-time-string) -4)
        " by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: " (current-time-string) "
;; Keywords: "
        ;; '(require 'finder)
        ;; ;;'(setq v1 (apply 'vector (mapcar 'car finder-known-keywords)))
        ;; '(setq v1 (mapcar (lambda (x) (list (symbol-name (car x))))
        ;;                   finder-known-keywords)
        ;;        v2 (mapconcat (lambda (x) (format "%10.0s:  %s" (car x) (cdr x)))
        ;;                      finder-known-keywords
        ;;                      "\n"))
        ;; ((let ((minibuffer-help-form v2))
        ;;    (completing-read "Keyword, C-h: " v1 nil t))
        ;;  str ", ") & -2 "
"

;; LICENSE GOES HERE

;;; Commentary:

;; " _ "

;;; Code:

(provide '" (file-name-sans-extension (file-name-nondirectory
(buffer-file-name))) ")

;;; " (file-name-nondirectory (buffer-file-name)) " ends here
")
auto-insert-alist)

;;}}}

;;{{{   `-- GNUS

(require 'gnus)

(setq gnus-novice-user nil)

(setq gnus-logo-color-style 'purp)
(setq gnus-logo-colors
      (cdr (assq gnus-logo-color-style gnus-logo-color-alist)))

(setq user-full-name "Zajcev Evgeny")

;; Allow '/' in group names
(setq gnus-invalid-group-regexp "[: `'\"]\\|^$")


(setq gnus-message-archive-group "Отправленные")

(setq gnus-summary-line-format
      (concat
       "%*%5{%U%R%z%}" ;%uc"
       "%4{|%}"
       "%-4k"
       "%4{|%}"
       "%2{%-8&user-date;%}" ; Datum
       "%4{|%}"
       "%2{ %}%(%-16,16F" ;From/To
       "%)%4{|%}"
       "%2{ %}%3{%B%}%1{%s%}\n"))

; gnus-face-1
(copy-face 'default 'mysubject)
(setq gnus-face-1 'mysubject)

;gnus-face-2
(copy-face 'default 'mytime)
(set-face-foreground 'mytime "indianred4")
(setq gnus-face-2 'mytime)

;gnus-face-3
;(copy-face 'ct-face1 'mythreads)
(copy-face 'default 'mythreads)
(set-face-foreground 'mythreads "indianred4")
(setq gnus-face-3 'mythreads)

;gnus-face-4
(copy-face 'default 'mygrey)
(set-face-foreground 'mygrey "grey")
(setq gnus-face-4 'mygrey)

;gnus-face-5
(copy-face 'default 'myblack)
(set-face-foreground 'myblack "grey60")
(setq gnus-face-5 'myblack)

;gnus-face-6
(copy-face 'default 'mybiggernumbers)
(set-face-foreground 'mybiggernumbers "indianred4")
(setq gnus-face-6 'mybiggernumbers)

(setq gnus-group-line-format
       "%6{%M%S%p    %}%(%2{%4y%}%4{|%}%s%-34,34G%3O%l %4{|%}%2{%4U.%}%4{|%}%2{%3T!%}%4{|%}%2{%3I?%}%4{|%}%2{%5t%} %)%4{| %}%1{%D%}\n")


(setq gnus-sum-thread-tree-root "+ ")
(setq gnus-sum-thread-tree-single-indent "* ")
(setq gnus-sum-thread-tree-vertical "| ")
(setq gnus-sum-thread-tree-indent "  ") ;; 3 Leerzeichen funktioniert
(setq gnus-sum-thread-tree-leaf-with-other "+-> ")
(setq gnus-sum-thread-tree-single-leaf "`-> ")


(setq gnus-sum-thread-tree-false-root "> ")
;(setq gnus-sum-thread-tree-root "")

; Darstellung von Followups
;(setq gnus-summary-same-subject ">")
(setq gnus-summary-same-subject "")

;(setq gnus-build-sparse-threads 'some)
;(setq gnus-fetch-old-headers 'some)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(add-hook 'gnus-group-prepare-hook 'highline-local-on)
(add-hook 'gnus-summary-prepare-hook 'highline-local-on)

(setq gnus-summary-default-high-score 500)
(setq gnus-summary-default-low-score -1)
(setq gnus-score-expiry-days 14
      gnus-save-score t
      bbdb/gnus-score-default 10
      gnus-score-thread-simplify t)

(setq gnus-article-decode-mime-words t)
(setq gnus-article-decode-charset 1)

                                        ;(require 'gnus-demon)
;; periodically check for new mail
(gnus-demon-add-handler 'gnus-group-get-new-news 3 nil)
;(gnus-demon-init)

;;; New mail notifier
(defvar lg-gnus-beepable-groups "target-team\\|JIRA"
  "A regexp that matches groups for which mail notification should take place.")

(defun lg-gnus-notify ()
  "Beep if we got mail in an interesting folder."
  (let ((case-fold-search t)
        (you-got-mail nil))
    (dolist (group nnmail-split-history)
      (message (format "%s" (caar group)))
      (when (string-match lg-gnus-beepable-groups (caar group))
        (setq you-got-mail t)))
    (when you-got-mail
      (beep)
      (sleep-for 0.2)
      (beep)
      (sleep-for 0.1)
      (beep))))

(add-hook 'gnus-after-getting-new-news-hook 'lg-gnus-notify)

;; Keywords header generation
;(autoload 'message-keyword-insert "messkeyw")
;(add-hook 'message-send-hook 'message-keyword-insert)

;; Make sure my followups are scored higher.
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)

;;}}}

;;{{{ `-- Webjump

;; <X-URL:http://www.neilvandyke.org/webjump/webjump-plus.el>
(setq webjump-sites
      `(
        ("Google" .
         [simple-query "www.google.com" "www.google.com/search?q=" ""])

        ("Yandex.RU" .
         [simple-query "www.ya.ru"
                       "http://www.yandex.ru/yandsearch?text="
                       "&rpt=rad"])
        ("Youtube.com" .
         [simple-query "www.youtube.com"
                       "http://www.youtube.com/results?search_query="
                       "&search_type=&aq=f"])

        ("MiniNova" .
         [simple-query "www.mininova.org"
                       "http://www.mininova.org/search/?search=" ""])

        ("CodeSearch" .
         [simple-query "www.google.com" "www.google.com/codesearch?q=" ""])

        ("Images" .
         [simple-query "images.google.com" "images.google.com/images?q=" ""])

        ("LispDoc" .
         [simple-query "lispdoc.com" "http://lispdoc.com/?q=" ""])

        ("Ru.Wikipedia.ORG" .
         [simple-query "ru.wikipedia.org"
                       "http://ru.wikipedia.org/wiki/Special:Search?search=" ""])

        ("En.Wikipedia.ORG" .
         [simple-query "en.wikipedia.org"
                       "http://en.wikipedia.org/wiki/Special:Search?search=" ""])

        ("Gramota.RU" .
         [simple-query
          "www.gramota.ru" "http://dic.gramota.ru/search.php?word="
          "&lop=x&gorb=x&efr=x&zar=x&ag=x&ab=x&lv=x&pe=x&az=x"])

        ("Price.RU" .
         [simple-query "www.price.ru"
                       "www.price.ru/bin/price/ctgrlist?pnam="
                       "&base=1&where=00"])

        ("Define" .
         [simple-query "www.google.com"
                       "www.google.com/search?q=define%3A" ""])

        ("RFC Editor" .
         [simple-query "www.rfc-editor.org"
                       "www.rfc-editor.org/cgi-bin/rfcsearch.pl?searchwords="
                       ,(concat "&opt=All%20Fields"
                                "&filefmt=txt"
                                "&search_doc=search_all"
                                "&match_method=prefix"
                                "&sort_method=newer"
                                "&num=25"
                                "&format=ftp")])

        ("MultiTran" . [simple-query "multitran.ru" "http://www.multitran.ru/c/m.exe?l1=2&l2=1&s=" ""])
        ("man" . [simple-query "linux.die.net" "https://linux.die.net/man/" ""])
        ))

(define-key global-map (kbd "C-c w j") 'webjump)

;; Chromium browser by default
(setq browse-url-browser-function 'browse-url-chromium)

(autoload 'browse-url-url-at-point "browse-url")

(defun lg-browse-url-at-point (&optional arg)
  (interactive "P")
  (let ((url (browse-url-url-at-point)))
    (if url
        (call-interactively 'browse-url-at-point)
      (call-interactively 'browse-url))))

(define-key global-map (kbd "C-c w w") 'lg-browse-url-at-point)

;;}}}

;;; Compilation
(setq compilation-scroll-output t)
(setq compilation-window-height 10)
(setq compilation-ask-about-save nil)   ; do not ask about saving

(defun lg-hide-compilation-buffer-on-success (buf msg)
  "On success compilation hide compilation buffer."
  (when (and (string= (buffer-name buf) "*compilation*")
             (string= msg "finished\n"))
    (when-let ((compwin (get-buffer-window buf)))
      (delete-window compwin))))

(add-hook 'compilation-finish-functions 'lg-hide-compilation-buffer-on-success)

;;; C-mode
(push (cons 'c-mode "bsd") c-default-style)

;; `#' aligns to left electrically
(setq c-electric-pound-behavior '(alignleft))

;; Make sure rdm/rc/rp are in PATH
(require 'subr-x)                       ; for `string-empty-p'
;(require 'rtags)

(autoload 'cmake-project--upward-find-last-file "cmake-project")

;; Default color of `rtags-argument-face' is eye bleeding
;; make it look natural
(copy-face 'eldoc-highlight-function-argument
           'rtags-argument-face)

(defun lg-ensure-rdm-is-running ()
  "Ensure `rdm' daemon is running."
  (when (and (featurep 'rtags) (not (get-process "rdm")) (executable-find "rdm"))
    (with-current-buffer (get-buffer-create " *rdm*")
      (start-process "rdm" (current-buffer) (executable-find "rdm")))))

(defun lg-rtags-apply-rc (dir)
  "Run rtag's `rc' utility on directory DIR."
  (lg-ensure-rdm-is-running)

  (when (and (featurep 'rtag) (executable-find "rc") (get-process "rdm"))
    (with-current-buffer (get-buffer " *rdm*")
      (rtags-call-rc "-J" dir))))

(defvar lg-cmake-build-tool-options " --no-print-directory")
(defvar lg-cmake-build-dir "build")

(defun lg-cmake-project-root-dir ()
  (let ((cmksrc (or (cmake-project--upward-find-last-file "CMakeLists.txt")
                    ;; parent and parent of parent as well
                    (cmake-project--upward-find-last-file
                     "CMakeLists.txt" (expand-file-name ".."))
                    (cmake-project--upward-find-last-file
                     "CMakeLists.txt" (expand-file-name "../.."))
                    )
                ))
    (and cmksrc (file-name-as-directory cmksrc))))

(defun lg-cmake-project-build-dir ()
  (let* ((prjroot (lg-cmake-project-root-dir))
         (buildir (and prjroot
                       (expand-file-name (concat prjroot lg-cmake-build-dir)))))
    (when (and buildir (file-exists-p buildir))
      buildir)))

(defun lg-compile (&optional target fallback)
  "cmake awared compile command."
  (interactive
   (when current-prefix-arg
     (list (read-string "Make target: ") nil)))

  (let ((cmk-build-dir (lg-cmake-project-build-dir)))
    (if cmk-build-dir
        (let ((compile-command
               (concat "cmake --build "
                       (shell-quote-argument cmk-build-dir)
                       " -- "
                       lg-cmake-build-tool-options
                       " " (or target ""))))
          (compile compile-command)
          (lg-rtags-apply-rc cmk-build-dir))

      (if fallback
          (funcall fallback)
        (call-interactively 'smart-compile)))))

(defun lg-compile-ctest-target ()
  "Compile 'test' target, assuming `ctest' will be executed."
  (interactive)
  (lg-compile "ARGS=\"--output-on-failure\" test"))

;;; cmake awared disassembler
(defun lg-file-name-reverse-split (fname)
  "Split filename's FNAME in reverse order."
  (unless (string-empty-p fname)
    (cons
     (file-name-nondirectory fname)
     (lg-file-name-reverse-split
      (substring (or (file-name-directory fname) "/") nil -1)))))

(defun lg-c-object-file (srcfile)
  "Find correspoinding object file for SRCFILE."
  (let ((prjdir (lg-cmake-project-root-dir)))
    (when (and prjdir (string-prefix-p prjdir srcfile))
      (let* ((relsrcfile (substring srcfile (length prjdir)))
             (reldir (file-name-directory relsrcfile))
             (objname (concat (file-name-nondirectory relsrcfile) ".o"))
             ;; TODO: multiple subdirectories support
             (outfile (expand-file-name
                       (concat prjdir lg-cmake-build-dir
                               "/" reldir "/CMakeFiles/"
                               (substring reldir nil -1) ".dir/" objname))))
        (message "Object file: %S / %S" outfile (file-exists-p outfile))
        (when (file-exists-p outfile)
          outfile)))))

(defun lg-disaster-objfile (srcfile obj-file line)
  ;; Cut&Paste from disaster.el
  (require 'disaster)

  (let ((dump (format "%s %s" disaster-objdump obj-file))
        (line-text (save-excursion
                     (buffer-substring-no-properties
                      (progn (beginning-of-line) (point))
                      (progn (end-of-line) (point)))))
        (asmbuf (get-buffer-create disaster-buffer-assembly)))

    (when (eq 0 (progn
                  (message (format "Running: %s" dump))
                  (shell-command dump asmbuf)))
      (with-current-buffer asmbuf
        ;; saveplace.el will prevent us from hopping to a line.
        (set (make-local-variable 'save-place) nil)
        (asm-mode)
        (disaster--shadow-non-assembly-code))
      (let ((oldbuf (current-buffer)))
        (switch-to-buffer-other-window asmbuf)
        (goto-char 0)
        (if (or (search-forward line-text nil t)
                (search-forward file-line nil t))
            (progn
              (recenter)
              (overlay-put (make-overlay (save-excursion
                                           (beginning-of-line)
                                           (point))
                                         (save-excursion
                                           (forward-line)
                                           (beginning-of-line)
                                           (point)))
                           'face 'region))
          (message "Couldn't find corresponding assembly line."))
        (switch-to-buffer-other-window oldbuf)))
    ))

(defun lg-disaster ()
  "Cmake awared disaster."
  (interactive)
  (let* ((srcfile (buffer-file-name))
         (line (line-number-at-pos))
         (objfile (lg-c-object-file srcfile)))
    (if objfile
        (lg-disaster-objfile srcfile objfile line)
      (disaster srcfile line))))

(defun lg-c-mode-customize ()
  ; enables electric-indent-mode
  ;  (c-toggle-electric-state t)

  ;; Nice rtag based eldoc
;  (lg-ensure-rdm-is-running)
;  (setq eldoc-documentation-function 'rtags-eldoc)

  (local-set-key (kbd "M-.") 'rtags-find-symbol)

  ;; switch .c <--> .h files
  (local-set-key (kbd "C-c C-h") 'ff-find-related-file)

  (local-set-key (kbd "C-c c") nil)
  (local-set-key (kbd "C-c c c") 'lg-compile)
  (local-set-key (kbd "C-c c t") 'lg-compile-ctest-target)
  )

(add-hook 'c-mode-hook 'lg-c-mode-customize)
(add-hook 'c++-mode-hook 'lg-c-mode-customize)

;;; Haskell mode

(defun hs-lint-compile (&optional prfxarg)
  "Run compile."
  (interactive "P")
  (lg-compile (concat "hlint" " " buffer-file-name)))

(defun lg-haskell-install-keys ()
  (define-key haskell-mode-map (kbd "C-c c c") 'hs-lint-compile)

  (define-key haskell-mode-map (kbd "C-c e r") 'lg-haskell-eval-region)
  (define-key haskell-mode-map (kbd "C-c e b") 'haskell-ghci-load-file)
;  (define-key haskell-mode-map (kbd "C-x C-e") 'lg-haskell-eval-phrase)
  )

(add-hook 'haskell-mode-hook 'lg-haskell-install-keys)

(define-key global-map (kbd "C-c d h") 'haskell-interactive-switch)


;;; Cmake mode
(defun lg-cmake-install-keys ()
  (local-set-key (kbd "C-c h") 'cmake-help))

(add-hook 'cmake-mode-hook 'lg-cmake-install-keys)

;;; ERC
(setq erc-track-enable-keybindings t)
(setq erc-nick "zevlg")

;;;{{{ `-- Nim language (package-install 'nim-mode)
(setq nim-indent-offset 4)              ; default 2 is too small

(setq nim-compile-default-command
  '("c" "-d:release" "--verbosity:0" "--listFullPaths" "--hint[Processing]:off"))

(defun lg-nim-compile ()
  "Run cmake or direct nim compilation."
  (interactive)
  (lg-compile nil #'nim-compile))

(defun lg-nim-mode-customize ()
  (set (make-local-variable 'compilation-read-command) nil)

  ;; Enable completion(M-<tab>)/find-symbol(M-.)/etc
  ;; requires `nimsuggest' (nimble install nimsuggest)
  (when nim-nimsuggest-path
    (nimsuggest-mode 1))

  (local-set-key (kbd "C-c c c") 'lg-nim-compile)
  )

(add-hook 'nim-mode-hook 'lg-nim-mode-customize)

;; Catch messages from nim compiler
;; Implies  --listFullPaths  nim flag
(setq nim-compilation-error-regexp
      `(nim ,(rx bol
                 (group-n 1 (0+ (not (any "("))))
                 (: "(")
                 (group-n 2 (regexp "[0-9]+"))
                 (: ", ")
                 (group-n 3 (regexp "[0-9]+"))
                 (: ")")
                 (| (group-n 4 (: " Warning:"))
                    (group-n 5 (: " Error:")))
                 (group-n 6 (* any))
                 eol
                 )
            1 2 3 (4 . nil)))
(add-to-list 'compilation-error-regexp-alist-alist
             nim-compilation-error-regexp)
(add-to-list 'compilation-error-regexp-alist 'nim)
;;;}}}

;;; Lua - https://github.com/immerrr/lua-mode.git
;; via MELPA (package-install 'lua-mode)
;(push "~/.emacs.d/thirdparty/lua-mode" load-path)
;(autoload 'lua-mode "lua-mode" "" t nil)
;(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
;(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(defalias 'run-lua #'lua-start-process)
(autoload 'lua-start-process "lua-mode" "" t nil)

(defun lg-lua-install-keys ()
  (local-set-key (kbd "C-c e r") 'lua-send-region)
  (local-set-key (kbd "C-c e b") 'lua-send-region)
  (local-set-key (kbd "C-c e s") 'lua-send-string)
  (local-set-key (kbd "C-j") 'lg-insert-nl-at-eol)
  )

(add-hook 'lua-mode-hook 'lg-lua-install-keys)

;;; Pony lang mode

(defun lg-pony-mode-customize ()
  (local-set-key (kbd "C-c c c") 'lg-compile)
  (local-set-key (kbd "C-c c t") 'lg-compile-ctest-target))

(add-hook 'ponylang-mode-hook 'lg-pony-mode-customize)

;;; MarkDown mode
;; https://raw.githubusercontent.com/defunkt/markdown-mode/master/markdown-mode.el
;;
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; ReStructured text mode
(autoload 'rst-mode "rst" "Major mode for editing reStructuredText" t)
(setq rst-mode-lazy nil)

;;; Diff-mode
(defun lg-diff-revert-hunk ()
  "Undo current hunk."
  (interactive)
  (let ((diff-advance-after-apply-hunk nil))
    (diff-apply-hunk '(4)))

  (diff-hunk-kill))

(defun lg-diff-install-keys ()
  (local-set-key (kbd "C-/") 'lg-diff-revert-hunk)
  (local-set-key (kbd "C-_") 'lg-diff-revert-hunk)
  )

(add-hook 'diff-mode-hook 'lg-diff-install-keys)

;;; Visible vertical line
(setq fci-rule-column 80
      fci-rule-width 2)
(setq fci-rule-color "gray75")

;;; Scad-mode
(add-to-list 'compilation-error-regexp-alist 'scad)

;; Define a regex to parse openscad's compilation message to jump over
;; error or warning points.
(add-to-list
 'compilation-error-regexp-alist-alist
 `(scad
   ,(rx line-start
        (group-n 4 "ERROR: ")
        "Parser error in line "
        (group-n 2 (1+ digit))
        ": syntax error\n"
         ;; filename
        "\nCan't parse file '"
        (group-n 1 (1+ (in alnum "\\" "/" "_" "." "-") "") ".scad")
        "'!"
        ;; Capture rest of message
        (0+ any) line-end)
   ;; See `compilation-error-regexp-alist's document for the detail
   1 2 3 (4 . 5)))

(autoload 'scad-preview--start "scad-preview" "Start preview scad.")

(setq scad-preview-window-position 'below)
(setq scad-preview-window-size 10)
(setq scad-preview-image-size '(900 . 450))
(setq scad-preview-colorscheme "Starnight")

(defun lg-scad-compile ()
  "Compile scad file to stl."
  (interactive)
  (let ((out-prefix (if (file-exists-p "stl/") "stl/" "")))
    (compile
     (concat scad-command " -o " out-prefix
             (file-name-base buffer-file-name) ".stl"
             " " buffer-file-name))
    ))

(defun lg-scad-mode-init ()
  (local-set-key (kbd "C-c c c") 'lg-scad-compile)
  (scad-preview--start))

(add-hook 'scad-mode-hook 'lg-scad-mode-init)

;;{{{ `-- Desktop

;(setq initial-buffer-choice t)

;;; Create *scratch-file* and make it unkillable
(defun lg-init-scratch-buffer ()
  (ignore-errors
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*scratch*")))

  (with-current-buffer
      (setq lsf-buffer (find-file-noselect lg-scratch-file))
    (font-lock-mode -1)
    (rename-buffer "*scratch*")))

;; Switch to scratch on startup, do it before loading `desktop',
;; because it adds hook into `after-init-hook'
(add-hook 'after-init-hook 'lg-init-scratch-buffer)

(require 'desktop)

;; Restoring frames does not work under EXWM
(setq desktop-restore-frames nil)
(setq desktop-restore-eager 5)          ; load fils lazily

(push 'dired-mode desktop-modes-not-to-save)
(push 'multran-mode desktop-modes-not-to-save)
(push 'image-mode desktop-modes-not-to-save)
(push 'telega-image-mode desktop-modes-not-to-save)
(push 'telega-root-mode desktop-modes-not-to-save)
(push 'telega-chat-mode desktop-modes-not-to-save)
(setq desktop-buffers-not-to-save
      "\\(^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS\\|^worklog\\)$")

(setq desktop-files-not-to-save
      (concat desktop-files-not-to-save "\\|\\.gz$\\|/rt/\\|/rt-dev/"))

;; Do not save files opened from telega
(defun lg-desktop-non-telega-file-p (filename bufname mode &rest rest)
  "Return non-nil if file is not a telega buffer."
  (when (fboundp 'telega-buffer-p)      ;checks telega is loaded
    (with-current-buffer bufname
      (not (or (telega-buffer-p)
               telega-edit-file-mode
               (eq major-mode 'telega-image-mode)
               telega--help-win-param)))))

(setq desktop-buffers-not-to-save-function #'lg-desktop-non-telega-file-p)

(setq desktop-globals-to-save
      '(desktop-missing-file-warning
        (kill-ring-yank-pointer . 50)
        (kill-ring . 50)

        tags-file-name
        tags-table-list
        register-alist
        (search-ring . 50)
        (regexp-search-ring . 50)
        (file-name-history . 50)
        (minibuffer-history . 50)
        (read-expression-history . 50)
        (extended-command-history . 50)
        (shell-command-history . 50)

        netcat-history
        multitran-read-history

        sudoku-custom-puzzles
        sudoku-solved-puzzles
        (command-history . 100)))

;; No autosaving please, causes problems
(setq desktop-auto-save-timeout nil)

(desktop-save-mode 1)

;;}}}

;;{{{ `-- Telega (telegram client)

(push "~/github/telega.el" load-path)
(push "~/github/telega.el/contrib" load-path)

(setq telega-adblock-sponsored-messages nil)

(setq telega-use-docker nil)
(setq telega-debug 'docker)

(setq telega-patrons-disable-sponsored-messages nil)
(setq telega-use-images t)

;; Open .webm files in external viewer
(add-to-list 'org-file-apps '("\\.webm\\'" . "mpv %s"))
(setcdr (assq t org-file-apps-gnu) 'browse-url-xdg-open)

(setcdr (assq 'file org-link-frame-setup) 'find-file)
(setq telega-open-file-function 'org-open-file)

;; c-mode for autogenerated .tl files
(add-to-list 'auto-mode-alist '("\\.tl\\'" . c-mode))

(autoload 'telega "telega" "Telegram client" t)
(define-key global-map (kbd "C-c t t") 'telega)

(setq telega-filter-custom-expand t)
(setq telega-use-tracking-for '(or unmuted mention))

;; (setq telega-animation-play-inline t)
;; (setq telega-video-note-play-inline t)
;; (setq telega-video-play-inline t)

(setq telega-root-fill-column 80)
(setq telega-chat-fill-column 70)

(setq telega-chat-show-deleted-messages-for '(not saved-messages))

;; Enlarged avatars for 2-lines view
(setq telega-avatar-factors-alist '((1 . (0.8 . 0.1))
                                    (2 . (0.8 . 0.1))))

(defun lg-telega-root-mode ()
  (hl-line-mode 1))

(add-hook 'telega-root-update-hook 'hl-line-highlight)
(add-hook 'telega-root-mode-hook 'lg-telega-root-mode)

(autoload 'telega-company-emoji "telega-company" "emoji backend" t)
(autoload 'telega-company-telegram-emoji "telega-company" "emoji backend" t)
(autoload 'telega-company-username "telega-company" "username backend" t)
(autoload 'telega-company-hashtag "telega-company" "hashtag backend" t)
(autoload 'telega-company-botcmd "telega-company" "botcmd backend" t)

;(setq telega-emoji-company-backend 'telega-company-telegram-emoji)

(defun lg-telega-chat-mode ()
  (set (make-local-variable 'company-backends)
       (append (list telega-emoji-company-backend
                     'telega-company-username
                     'telega-company-hashtag)
               (when (telega-chat-bot-p telega-chatbuf--chat)
                 '(telega-company-botcmd))))
  (company-mode 1)

  ;; (make-face 'my-telega-face)
  ;; (set-face-attribute 'my-telega-face nil :font "DejaVu Sans Mono")
  ;; (setq buffer-face-mode-face 'my-telega-face)
  ;; (buffer-face-mode)
  )

(add-hook 'telega-chat-mode-hook 'lg-telega-chat-mode)

(defun lg-telega-open-notes ()
  "Open NOTES.org from the \"Saved Messages\""
  (interactive)
  (message "telega: Opening NOTES.org..")
  (telega-msg-get (telega-chat-me) 185013895168
    (lambda (msg &optional _offline-p)
      (cl-assert (telega-msg-type-p 'messageDocument msg))
      (telega-msg-open-document msg))))

(defun lg-telega-load ()
  (require 'telega-dashboard)
  (add-to-list 'dashboard-items '(telega-chats . 5))

  (require 'telega-stories)
  (telega-stories-mode 1)
  (add-to-list 'dashboard-items '(telega-stories . 5))
  (define-key telega-root-mode-map (kbd "v e") 'telega-view-emacs-stories)

  (require 'telega-url-shorten)
  (global-telega-url-shorten-mode)

  ;; `C-c o l' to copy telega links
  (require 'ol-telega)

  ;; Code highlighting
  (require 'telega-mnz)
  (global-telega-mnz-mode 1)

  ;; Blocking adverts in channels
  (require 'telega-adblock)
  (setq telega-adblock-chat-order-if-last-message-ignored "1")
  (telega-adblock-mode 1)

  (setq telega-chat-title-custom-for
        (list (cons '(or saved-messages replies-messages)
                    (lambda (title)
                      (concat (propertize "🎗" 'face 'telega-blue)
                              (propertize title 'face 'bold))))))

  (cl-pushnew "@fmusbot" telega-known-inline-bots)
  (cl-pushnew "@emojimixbot" telega-known-inline-bots)
  (define-key global-map (kbd "C-c t") telega-prefix-map)

;  (define-key telega-prefix-map (kbd "n") 'lg-telega-open-notes)

;  (telega-mode-line-mode 1)

  (setq telega-completing-read-function 'ido-completing-read)
  (when (telega-x-frame)
;    (setq telega-symbol-eliding "…")
;    (telega-symbol-set-width telega-symbol-eliding 2)
    )

  ;; Install custom symbols widths
  ;; (dolist (c2 '("♥" "🎗" "∏" "∑" "★" "🛠"
  ;;               "🇦" "🇧" "🇨" "🇩" "🇪" "🇫" "🇯" "🇰"
  ;;               "🇱" "🇲" "🇳" "🇴" "🇵" "🇶" "🇷" "🇸"
  ;;               "🇹" "🇺" "🇻" "🇼" "🇽" "🇾" "🇿"))
  ;;   (telega-symbol-set-width c2 2))
  ;; (telega-symbol-set-width (cons 127344 127384) 2)

  )

(add-hook 'telega-load-hook 'lg-telega-load)
(add-hook 'telega-load-hook 'telega-mode-line-mode)
(add-hook 'telega-load-hook 'telega-appindicator-mode)
(add-hook 'telega-load-hook 'telega-autoplay-mode)
(add-hook 'telega-load-hook 'global-telega-squash-message-mode)
(add-hook 'telega-load-hook 'telega-my-location-mode)

(add-hook 'telega-open-file-hook 'telega-edit-file-mode)

;;}}}
;;{{{ `-- Vterm

(use-package vterm
  :load-path "~/dev/emacs-libvterm"
  :init
  (setq vterm-shell "screen -DR")
  (setq vterm-max-scrollback 0)

  (setq vterm-disable-underline t)
  (setq vterm-disable-inverse-video t)
  (setq vterm-keymap-exceptions
        '("C-c" "C-x" "C-g" "C-h" "C-l" "M-x" "C-y" "M-y" "C-M-l"))
  :commands (vterm)
  :config
  (defface lg-vterm-face
    '((t :family "DejaVu Sans Mono" :height 0.5
         :foreground "green" :background "black"))
    "Face for my vterms.")

  (set-face-foreground 'vterm-color-underline "yellow")
  (set-face-background 'vterm-color-inverse-video "green4")
  (set-face-background 'vterm-color-black "gray20")
  (set-face-background 'vterm-color-blue "DodgerBlue3")
  (set-face-foreground 'vterm-color-blue "royal blue")

  (define-key vterm-mode-map (kbd "S-<prior>") 'vterm--self-insert)
  (define-key vterm-mode-map (kbd "C-<up>") 'vterm--self-insert)
  (define-key vterm-mode-map (kbd "S-<next>") 'vterm--self-insert)
  (define-key vterm-mode-map (kbd "C-<down>") 'vterm--self-insert)
  (define-key vterm-mode-map (kbd "C-<SPC>") 'lg-vterm-copy-mode-set-mark)
  (define-key vterm-mode-map (kbd "C-c C-x") 'vterm-send-C-x)
  (define-key vterm-mode-map (kbd "C-c M-x") 'vterm-send-M-x)

  (defun lg-on-vterm-mode ()
    (setq buffer-face-mode-face 'lg-vterm-face)
    (buffer-face-mode 1))

  (add-hook 'vterm-mode-hook 'lg-on-vterm-mode))

(defun lg-vterm-copy-mode-set-mark ()
  "Enter vterm-copy-mode and set mark at point."
  (interactive)
  (vterm-copy-mode)
  (set-mark-command nil))

(defun lg-switch-to-vterm (arg &optional shell-cmd)
  (interactive "p")
  (let* ((bufname (concat "vterm" (number-to-string arg)))
         (vterm-buf (get-buffer bufname)))
    (if vterm-buf
        (switch-to-buffer vterm-buf)

      (let ((vterm-shell (or shell-cmd vterm-shell)))
        (vterm bufname)))))

(defun lg-switch-to-vterm2 ()
  (interactive)
  (lg-switch-to-vterm 2 "zsh"))

(define-key global-map (kbd "C-c v") #'lg-switch-to-vterm)
(define-key global-map (kbd "C-c V") #'lg-switch-to-vterm2)


;; Viewing asciinema with vterm
(defun lg-asciinema-done (buffer &rest _ignored)
  (when buffer
    (message "asciinema: play done! Press `q' to close"))
  )

(defun lg-asciinema-view (url &optional speed)
  (let ((vterm-shell (concat "asciinema play -s "
                             (number-to-string (or speed 2))
                             " " url)))
    (vterm "asciinema")
    (text-scale-decrease 2)

    (setq-local vterm-kill-buffer-on-exit nil)
    (add-hook 'vterm-exit-functions 'lg-asciinema-done nil 'local)

    (local-set-key (kbd "q") 'quit-window)
    (message "asciinema: Press `q' to quit")))

(defun lg-maybe-asciinema-view (origfunc url &optional in-web-browser)
  "Maybe open link to asciinema.org in vterm."
  (if (and (not in-web-browser)
           (string-match "https?://asciinema.org" url))
      (lg-asciinema-view url)
    (funcall origfunc url in-web-browser)))

(advice-add 'telega-browse-url :around 'lg-maybe-asciinema-view)

;; Char-mode for vterm

;;}}}

;;{{{ `-- Clojure / CIDER

(use-package cider
  :init
  ;; TODO
  )

;;}}}

;;{{{ `-- Go mode

;; No headerline please
(setq lsp-headerline-breadcrumb-enable nil)

(add-to-list 'exec-path (expand-file-name "~/go/bin"))

(defun lg-on-go-mode ()
;  (yas-minor-mode-on)
  (lsp-deferred)

  (local-set-key (kbd "C-c c c") 'go-errcheck)

  ;; Set up before-save hooks to format buffer and add/delete imports.  Make
  ;; sure you don't have other gofmt/goimports hooks enabled.
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook 'lg-on-go-mode)

;;}}}

;;{{{ `-- Misc modes customization

;; Convert "unknown" image formats with external converter
(setq image-use-external-converter t)

;;; Gnuplot mode
(use-package gnuplot
  :init
  (setq auto-mode-alist
        (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))
  :bind (:map gnuplot-mode-map
              ("C-c C-s" . gnuplot-show-gnuplot-buffer)
              ("C-x C-e" . gnuplot-send-line-to-gnuplot)
              ("C-c e r" . gnuplot-send-region-to-gnuplot)
              ("C-c e b" . gnuplot-send-buffer-to-gnuplot)))

;;}}}

;;{{{ `-- Editing tools

;;; Colorize modeline when defining macro
(defvar lg-minibuffer-color-saved
  (face-background 'mode-line))

(defvar lg-minibuffer-color-when-defining-macro "deeppink"
  "Color to use for minibuffer when defining macro.
I hate this color, so i wont forget to finish macro wheen needed.")
(defun lg-colorize-minibuffer (color)
  (setq lg-minibuffer-color-saved
        (face-background 'mode-line))
  (set-face-background 'mode-line color))

(defadvice kmacro-start-macro (before lg-colorize-minibuffer activate)
  "Start colorizing minibuffer untill macro is defined."
  (unless (string= (face-background 'mode-line)
                   lg-minibuffer-color-when-defining-macro)
    (lg-colorize-minibuffer lg-minibuffer-color-when-defining-macro)))

(defadvice kmacro-end-macro (before lg-decolorize-minibuffer activate)
  "Macro defined, stop colorizing minibuffer."
  (lg-colorize-minibuffer lg-minibuffer-color-saved))

  (if defining-kbd-macro
      (kmacro-end-macro nil))

(defun lg-end-and-call-macro (arg)
  "Run `apply-macro-to-region-lines' if region is active.
Or run `call-last-kbd-macro' otherwise."
  (interactive "P")
  (if defining-kbd-macro
      (kmacro-end-macro nil))

  (if (region-active-p)
      (call-interactively #'apply-macro-to-region-lines)
    (call-interactively #'kmacro-call-macro)))

(define-key global-map (kbd "<f9>") 'lg-end-and-call-macro)
(define-key global-map (kbd "C-x e") 'lg-end-and-call-macro)

;;}}}

;;{{{ `-- gif screencast
(defun lg-xrectsel ()
  "Select rectangle on screen."
  (shell-command-to-string "xrectsel"))

(setq gif-screencast-program "import")
(setq gif-screencast-args nil)

(defun lg-screencast (cap-geometry)
  "Start recording gif screecast."
  (interactive (list (lg-xrectsel)))

  (setq gif-screencast-args
        (list "-window" "root" "-crop" cap-geometry))
  (gif-screencast)
  )

;;}}}

;;;{{{ `-- Clojure

(add-hook 'cider-repl-mode-hook 'company-mode)
(add-hook 'clojure-mode-hook 'company-mode)
(add-hook 'clojure-mode-hook 'cider-mode)

(define-key clojure-mode-map (kbd "C-c e b") 'cider-eval-buffer)
(define-key clojure-mode-map (kbd "C-c e r") 'cider-eval-region)
(define-key clojure-mode-map (kbd "C-c e f") 'cider-eval-defun-at-point)
(define-key clojure-mode-map (kbd "C-c e e") 'cider-macroexpand-1)

(define-key clojure-mode-map (kbd "C-c d e") 'cider-switch-to-repl-buffer)

;;;}}}

;;;{{{ `-- Tramp
;; Accept fancy colored prompts
(setq tramp-shell-prompt-pattern
      "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
;;;}}}

;; favorite unicode chars
(define-key global-map (kbd "C-x 8 0") (kbd "°"))
(define-key global-map (kbd "C-x 8 r") (kbd "₽"))
(define-key global-map (kbd "C-x 8 e") (kbd "€"))

;; Backing up in single directory
(setq backup-directory-alist
      (list (cons (expand-file-name ".*" "~")
                  (expand-file-name "backups" user-emacs-directory))))

;;; Work specific emacs setup
(ignore-error 'file-missing
  (load-library "work.el"))

;;; Home specific stuff
(ignore-error 'file-missing
  (load-library "home.el"))

(ignore-errors
  (when (getenv "WITH_EXWM")
    (let ((exwm-debug-on t))
;      (load-library "exwmrc")
      (require 'exwm)
      (require 'exwm-config)
      (exwm-config-example)

      ;; Enable system tray
      (require 'exwm-systemtray)
      (exwm-systemtray-enable)

      ;; Enable debugging
      (exwm-debug 1)
      ;; Enable EXWM for non-tty emacs
      ;; (when window-system
      ;;   (exwm-enable))
      )))

;; Dashboard
(use-package dashboard
  :init
  (setq dashboard-image-banner-max-height 64)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-set-footer nil)
  (setq dashboard-set-heading-icons t)

  )

(use-package dockerfile-mode
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile.*\\'" . dockerfile-mode))
  )

;; (use-package yascroll
;;   :ensure t
;;   :config
;;   (setq yascroll:delay-to-hide nil)
;;   (setq yascroll:scroll-bar '(right-fringe))
;;   (global-yascroll-bar-mode 1)
;;   )

(use-package mlscroll
  :ensure t
  :config
  (setq mlscroll-in-color "gray70")
  (setq mlscroll-out-color "gray85")
  (setq mlscroll-minimum-current-width 4)
  (setq mlscroll-border 2)
  (mlscroll-mode 1))

(use-package sudo-edit
  :config (sudo-edit-indicator-mode)
  :commands (sudo-edit))

;;; Fonts setup
(set-fontset-font t 'symbol "Symbola" nil 'prepend)
;(set-fontset-font t 'unicode "Symbola" nil 'append)

;(set-fontset-font t 'unicode "AllTheIcons" nil 'append)
(set-fontset-font t 'unicode "all-the-icons" nil 'append)
(set-fontset-font t 'unicode "file-icons" nil 'append)
(set-fontset-font t 'unicode "github-octicons" nil 'append)
;(set-fontset-font t 'unicode "FontAwesome" nil 'prepend)

(set-fontset-font t 'mathematical "DejaVu Serif" nil 'append)

;;; Profiler setup
(setq profiler-report-cpu-line-format
      '((10 right ((5 right) (5 right)))
        (1 left "%s")
        (0 left)))

(setq profiler-report-memory-line-format
  '((100 left)
    (19 right ((14 right profiler-format-number)
               (5 right)))))
(put 'upcase-region 'disabled nil)

;;; Reverse-im

;; Taken from https://github.com/xFA25E/cyrillic-dvorak-im/blob/master/cyrillic-dvorak-im.el
(require 'quail)

(quail-define-package
 "cyrillic-dvorak" "Cyrillic" "ЙЦУК" nil
 "ЙЦУКЕН keyboard layout widely used in Russia (ISO 8859-5 encoding)
  in assuming that your default keyboard layout is dvorak"
 nil t t t t nil nil nil nil nil t)

;;  1! 2" 3' 4; 5 6: 7? 8 9( 0) -_ =+ Ё
;;   Й  Ц  У  К  Е  Н  Г  Ш  Щ  З  Х  ъ
;;    Ф  Ы  В  А  П  Р  О  Л  Д Ж  Э
;;     Я  Ч  С  М  И  Т  Ь  Б  Ю  .,

(quail-define-rules
 ("1" ?1) ("2" ?2) ("3" ?3) ("4" ?4) ("5" ?5) ("6" ?6) ("7" ?7) ("8" ?8) ("9" ?9) ("0" ?0) ("[" ?-) ("]" ?=)
 ("`" ?ё) ("'" ?й) ("," ?ц) ("." ?у) ("p" ?к) ("y" ?е) ("f" ?н) ("g" ?г) ("c" ?ш) ("r" ?щ) ("l" ?з) ("/" ?х) ("=" ?ъ)
 ("a" ?ф) ("o" ?ы) ("e" ?в) ("u" ?а) ("i" ?п) ("d" ?р) ("h" ?о) ("t" ?л) ("n" ?д) ("s" ?ж) ("-" ?э) ("\\" ?\\) (";" ?я)
 ("q" ?ч) ("j" ?с) ("k" ?м) ("x" ?и) ("b" ?т) ("m" ?ь) ("w" ?б) ("v" ?ю) ("z" ?.)

 ("!" ?!) ("@" ?\") ("#" ?') ("$" ?\;) ("%" ?%) ("^" ?:) ("&" ??) ("*" ?*) ("(" ?\() (")" ?\)) ("{" ?_) ("}" ?+) ("~" ?Ё) ("\"" ?Й)
 ("<" ?Ц) (">" ?У) ("P" ?К) ("Y" ?Е) ("F" ?Н) ("G" ?Г) ("C" ?Ш) ("R" ?Щ) ("L" ?З) ("?" ?Х) ("+" ?Ъ)
 ("A" ?Ф) ("O" ?Ы) ("E" ?В) ("U" ?А) ("I" ?П) ("D" ?Р) ("H" ?О) ("T" ?Л) ("N" ?Д) ("S" ?Ж) ("_" ?Э) ("|" ?/) (":" ?Я)
 ("Q" ?Ч) ("J" ?С) ("K" ?М) ("X" ?И) ("B" ?Т) ("M" ?Ь) ("W" ?Б) ("V" ?Ю) ("Z" ?,))

(use-package reverse-im
  :ensure t
  :custom
  (reverse-im-input-methods '("cyrillic-dvorak"))
  :config
  (reverse-im-mode t))
