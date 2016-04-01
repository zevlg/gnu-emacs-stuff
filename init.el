;; init.el --- Custom configuration for GNU Emacs.
;;
;; Copyright (C) 2015 by Zajcev Evgeny.
;;
(set-frame-height nil 30)
;;(require 'package)
;;(add-to-list 'package-archives
;;             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(package-initialize)
(server-start)

(push "~/.emacs.d/lisp" load-path)
(push "~/.emacs.d/thirdparty" load-path)
(push "~/dev/gnu-emacs-stuff" load-path)
(push "/usr/share/emacs24/site-lisp/git" load-path)
(autoload 'git-status "git" "git-status" t)
(load-library "xemacs-theme-source-code")
(custom-theme-set-faces
 'xemacs
 `(escape-glyph ((t (:weight bold :background "gold" :foreground "blue"
                             :box (:line-width -1 :color "black"))))))

;;
;(set-face-font
; 'default "-xos4-terminus-medium-r-normal--32-320-72-72-c-160-koi8-r")

(set-face-attribute 'default nil :family "Inconsolata LGC")
(set-face-attribute 'default nil :height 240)

(setq inhibit-splash-screen t)
(setq enable-recursive-minibuffers t)
(setq select-enable-primary t)

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

(mouse-avoidance-mode 'none)
(blink-cursor-mode 0)
(set-cursor-color "red3")

(tooltip-mode -1)
(setq show-help-function nil)

(setq dabbrev-ignored-buffer-names
      '(" *Message-Log*" "*Messages*" "*Buffer List*" "*Ibuffer*"))

(define-key global-map (kbd "C-<tab>") 'other-window)

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
(require 'comint)
(define-key comint-mode-map (kbd "C-M-l") 'switch-to-other-buffer)

;;; Use `iswitchb' instead of ugly `switch-to-buffer'.
(setq ido-max-window-height 1)
(ido-mode 1)

;; M-x package-install RET undo-tree RET
;;; git clone http://www.dr-qubit.org/git/undo-tree.git
;;(push "~/.emacs.d/undo-tree" load-path)
(autoload 'undo-tree-undo "undo-tree" "Undo from redo package." t)
(autoload 'undo-tree-redo "undo-tree" "Redo from redo package." t)

(global-set-key (kbd "C-/") 'undo-tree-undo)
(global-set-key (kbd "C-x C-/") 'undo-tree-redo)

;;; Use cool `ibuffer' instead of ugly `list-buffers'
(define-key global-map (kbd "C-x C-b") 'ibuffer)

(setq mouse-yank-at-point t)

(defun lg-mouse-yank ()
  "As `mouse-yank', but does not require to be bound to mouse."
  (interactive)
  (mouse-yank-at-click nil nil))

;;; Paren mode
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

(set-face-background 'show-paren-match "#CCDDCC")
(set-face-background 'show-paren-mismatch "#FFFF00")
(set-face-foreground 'show-paren-mismatch nil)

(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(setq show-paren-priority -1000)
(show-paren-mode 1)

;;; Selected region
;(set-face-background 'default "gray80")
(set-face-background 'region "gray65")

(setq scroll-error-top-bottom t)

;; Kill \n also if killing from the begining of line
(setq kill-whole-line t)

;; NOTE: With negative prefix arg `kill-line' will kill lines backward!
(defun lg-kill-line (&optional arg)
  "Deletes to the end of current line.
If ARG is given `lg-kill-line' deletes to the beginning of line."
  (interactive "P")
  (if (and arg (listp arg))
      (delete-region (point-at-bol) (point))
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

(define-key global-map (kbd "C-k") 'lg-kill-line)
(define-key global-map (kbd "C-w") 'lg-kill-region)

;; Killing bindings
(define-key global-map (kbd "M-<f4>") 'lg-kill-current-buffer)
(define-key global-map (kbd "C-x k") 'lg-kill-current-buffer)

;;{{{ `-- Whitespace

(require 'whitespace)
(set-face-background 'whitespace-tab "yellow")
(set-face-background 'whitespace-space "LightBlue1")
(set-face-background 'whitespace-indentation "skyblue")

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

(defvar lg-scratch-file (expand-file-name "~/.sxemacs/*scratch-file*"))

(defun lg-switch-to-scratch (arg)
  "Switch to \\*scratch\\* buffer.
If prefix ARG is specified, switch in other window."
  (interactive "P")
  (let ((scbuf (find-file lg-scratch-file)))
    (if arg
        (switch-to-buffer-other-window scbuf)
      (switch-to-buffer scbuf))))

(setq initial-major-mode 'lisp-interaction-mode)
(push '("\\*scratch-file\\*$" . lisp-interaction-mode) auto-mode-alist)

;;; Create *scratch-file* and make it unkillable
;(add-hook 'term-setup-hook (lambda () (kill-buffer "*scratch*")))
(with-current-buffer
    (setq lsf-buffer (find-file-noselect lg-scratch-file))
  (kill-buffer "*scratch*")
  (rename-buffer "*scratch*"))

(defun lg-save-lsf-buffer ()
  "Save *scratch-file* buffer on exit."
  (when (buffer-live-p lsf-buffer)
    (with-current-buffer lsf-buffer
      (save-buffer))))

;; for slippery fingers
(defun lg-ask-exit-emacs (arg)
  "Ask for confirmation before exit.
If used with prefix ARG, force Emacs to exit, skiping `kill-emacs-hook'."
  (interactive "P")
  (if arg
      (let (kill-emacs-hook)
        (kill-emacs))

    (when (yes-or-no-p "Exit Emacs?")
      (lg-save-lsf-buffer)
      (save-buffers-kill-emacs))))

(define-key global-map (kbd "M-<f3>") 'lg-switch-to-scratch)
(define-key global-map (kbd "C-<f3>") 'lg-switch-to-scratch)
(define-key global-map (kbd "C-c C-s") 'lg-switch-to-scratch)

(defun lg-insert-nl-at-eol (arg)
  "Insert new line at the end of line.
If prefix ARG is supplied, do not move point."
  (interactive "P")
  (eval (list (if arg 'save-excursion 'progn)
              '(end-of-line)
              '(newline-and-indent))))

(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "C-j") 'lg-insert-nl-at-eol)

;; To join two lines (aka vi's J)
(define-key global-map (kbd "C-^") (kbd "C-u M-^"))

(define-key global-map (kbd "C-l") 'recenter)

;;; For quick region commenting
(define-key global-map (kbd "C-c C-c") 'comment-region)
(define-key global-map (kbd "C-c ;") 'comment-region)

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
   (list (read-from-minibuffer "Enter expression: ")
         current-prefix-arg))

  (let ((result (calc-eval expr)))
    (if arg
        (insert result)
      (message "Result: [%s] = %s" expr result))))
(global-set-key (kbd "M-#") 'lg-mini-calc)

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
(setq scroll-preserve-screen-position t)

(define-key global-map (kbd "M-p") 'scroll-down-command)
(define-key global-map (kbd "M-n") 'scroll-up-command)

;;}}}

;;{{{ `-- Minibuffer

(setq echo-keystrokes 0.1)
(setq resize-mini-windows nil)

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

;;{{{   `-- C-cf - Prefix for Finding commands

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

;;{{{ `-- ELIM

;; for prpl-mra
(ignore-errors
  (add-to-list 'load-path "~/.emacs.d/elim/elisp")
  (load-library "garak")

  (tracking-mode 1))

;;}}}

(defconst lg-square-64x64-xpm
  (concat
   "/* XPM */\n"
   "static char *mini_square_xpm[] = {\n"
   "/* columns rows colors chars-per-pixel */\n"
   "\"64 64 2 1\",\n"
   "\"       c None s background\",\n"
   "\".      c gray77 s grido\",\n"
   "/* pixels */"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"................................................................\"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n,"
   "\"    .                                                           \"\n"
   "\"    .                                                           \"\n"
   "\"    .                                                           \"\n"
   "\"    .                                                           \"\n"
   "\"    .                                                           \"\n"
   "\"    .                                                           \"\n"
   "\"    .                                                           \"\n"
   "\"    .                                                           \"\n"
   "\"    .                                                           \"\n"
   "};"))

(defconst lg-square-64x64-xbm
  (concat
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\x00\x00\x00\x00\x00\x00\x00\x00\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF"
"\xFF\xFF\xFF\xFF\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
"\xEF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
))

;(set-face-background-pixmap
; 'default nil)
; (list 64 64 lg-square-64x64-xpm)
; )

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
                 (flet ((insert-before-markers (&rest args)
                                               (apply #'insert args)))
                   (skeleton-insert
                    (cons nil (cdddr pair))))))))))

(define-key global-map (kbd "C-M-{") 'backward-paragraph)
(define-key global-map (kbd "C-M-}") 'forward-paragraph)
(define-key global-map (kbd "C-M-'") 'abbrev-prefix-mark)

(define-key global-map (kbd "M-\"") 'lg-skeleton-pair-insert)
(define-key global-map (kbd "M-`") 'lg-skeleton-pair-insert)
(define-key global-map (kbd "M-'") 'lg-skeleton-pair-insert)
(define-key global-map (kbd "M-{") 'lg-skeleton-pair-insert)
(define-key global-map (kbd "M-(") 'lg-skeleton-pair-insert)
(define-key global-map (kbd "M-[") 'lg-skeleton-pair-insert)

;;}}}

;;{{{ `-- Highlight current line

(setq highline-priority 1000)

;;; Highlight current line highline.el customization
(autoload 'highline-local-on "highline" nil nil)
(autoload 'highline-mode "highline" nil t)

(defface highlight-line-face
  '((((class color) (background dark))
     (:background "navy"))
    (((class color) (background light))
     (:background "greenyellow"))
    (t (:background "greenyellow")))
  "Face for highline mode.")
(setq highline-face 'highlight-line-face)

;;; Use highline in several major modes by default
(add-hook 'ibuffer-hooks 'highline-local-on)
(add-hook 'cvs-mode-hook 'highline-local-on)

(add-hook 'gnus-group-prepare-hook 'highline-local-on)
(add-hook 'gnus-summary-prepare-hook 'highline-local-on)

;;}}}

;;{{{ `-- Dot-mode
;; http://www.emacswiki.org/emacs/download/dot-mode.el
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
    (make-local-hook 'pre-command-hook)
    (make-local-hook 'post-command-hook)
    (make-local-hook 'after-change-functions)
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

;;}}}

;;{{{ `-- Emacs lisp mode

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

;; NOTE: Installs C-ce Prefix for elisp operations (for lisp-mode only)
(define-key emacs-lisp-mode-map (kbd "C-c e b") 'lg-emacs-eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c e r") 'lg-emacs-eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c e f") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c e s") 'eval-last-sexp)
;; NOTE: There is also `emacs-lisp-macroexpand'
(define-key emacs-lisp-mode-map (kbd "C-c e e") 'pp-macroexpand-last-sexp)

(define-key lisp-interaction-mode-map (kbd "C-c e b") 'lg-emacs-eval-buffer)
(define-key lisp-interaction-mode-map (kbd "C-c e r") 'lg-emacs-eval-region)
(define-key lisp-interaction-mode-map (kbd "C-c e f") 'eval-defun)
(define-key lisp-interaction-mode-map (kbd "C-c e s") 'eval-last-sexp)
(define-key lisp-interaction-mode-map (kbd "C-c e e") 'pp-macroexpand-last-sexp)

;;}}}

;;{{{ `-- Editing commands

;; C-cd Prefix for DICT
(define-key global-map (kbd "C-c d d") 'dict)
(define-key global-map (kbd "C-c d r") 'rdict)
(define-key global-map (kbd "C-c d t") 'google-translate-region)

(defun lg-py-shell ()
  "Switch to python interpreter."
  (interactive)
  (unless (python-shell-get-process)
    (run-python))
  (python-shell-switch-to-shell))

(define-key global-map (kbd "C-c d p") 'lg-py-shell)

;; C-ce Editing prefix
(define-key global-map (kbd "C-c e c") 'checkdoc)
(define-key global-map (kbd "C-c e w") 'whitespace-cleanup)
;;(define-key global-map (kbd "C-c e l") 'lg-fix-long-lines)
;;(define-key global-map (kbd "C-c e i") 'lg-ispell-region-or-buffer)
;;(define-key global-map (kbd "C-c e y") 'yofy)

;; C-cr - Prefix for REGULAR EXPRESSIONS
(define-key global-map (kbd "C-c r b") 're-builder)
(define-key global-map (kbd "C-c r s") 're-search-forward)
(define-key global-map (kbd "C-c r r") 're-search-backward)
(define-key global-map (kbd "C-c r e") 'query-replace-regexp)
(define-key global-map (kbd "C-c r g") 'grep)
(define-key global-map (kbd "C-c r o") 'occur)

;; C-cm Prefix for MISC commands
(define-key global-map (kbd "C-c m c") 'lg-mini-calc)
(define-key global-map (kbd "C-c m f") 'folding-mode)
(define-key global-map (kbd "C-c m a") 'ascii-display)
(define-key global-map (kbd "C-c m g") 'imenu)
(define-key global-map (kbd "C-c m o") 'occur)
(define-key global-map (kbd "C-c m |") 'vertical-mode)
(define-key global-map (kbd "C-c m v") 'vvb-mode)
(define-key global-map (kbd "C-c m h") 'highline-mode)
(define-key global-map (kbd "C-c m w") 'whitespace-mode)
(define-key global-map (kbd "C-c m F") 'flyspell-mode)
(define-key global-map (kbd "C-c m i") 'flyspell-mode)
(define-key global-map (kbd "C-c m m") 'himarks-mode)
(define-key global-map (kbd "C-c m s") 'stripes-mode)
(define-key global-map (kbd "C-c m l") 'lg-column-ruler)
(define-key global-map (kbd "C-c m d") 'dot-mode)
(define-key global-map (kbd "C-c m r") 'rst-mode)
(define-key global-map (kbd "C-c m <RET>") 'hide-cr-mode)
(define-key global-map (kbd "C-c m p") 'pabbrev-mode)

;;}}}

;;{{{   `-- C-cl - Prefix for Listing commands

(define-key global-map (kbd "C-c l a") 'list-abbrevs)
(define-key global-map (kbd "C-c l b") 'list-bookmarks)
(define-key global-map (kbd "C-c l h") 'list-command-history)
(define-key global-map (kbd "C-c l c") 'list-colors-display)
(define-key global-map (kbd "C-c l f") 'list-faces-display)
(define-key global-map (kbd "C-c l i") 'list-itimers)
(define-key global-map (kbd "C-c l m") 'list-matching-lines)
(define-key global-map (kbd "C-c l t") 'list-text-properties-at)
(define-key global-map (kbd "C-c l p") 'list-processes)
(define-key global-map (kbd "C-c l s") 'list-strokes)
(define-key global-map (kbd "C-c l .") 'list-tags)
(define-key global-map (kbd "C-c l k") 'browse-kill-ring)
(define-key global-map (kbd "C-c l r") 'list-registers)
(define-key global-map (kbd "C-c l o") 'list-packages)

;;}}}

;;{{{   `-- C-cc - Prefix for Count commands

(define-key global-map (kbd "C-c c w") 'count-words)
(define-key global-map (kbd "C-c c m") 'count-matches)

;;}}}

;;{{{   `-- C-cg - Git commands

(define-key global-map (kbd "C-c g s") 'git-status)

;;}}}

;;{{{ `-- Python

;; Builtint `python-mode' is slow in emacs 25, so use this one

(push "~/.emacs.d/python-mode" load-path)
(require 'python-mode)

(setq elpy-modules '(elpy-module-sane-defaults
                     elpy-module-company
                     elpy-module-eldoc
                     elpy-module-flymake
                     elpy-module-pyvenv))
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

(define-key py-shell-map (kbd "M-C-l") 'switch-to-other-buffer)

;;; Cython mode
(require 'cython-mode)

;; For scons
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))

;;}}}

;; Save places of visited files
(require 'saveplace)
(setq save-place-file (expand-file-name "~/.emacs-places"))
(save-place-mode 1)

(defun emacs-places-save ()
  "Save places to file."
  (interactive)
  (save-place-kill-emacs-hook))

;;
(icomplete-mode)
(setq icomplete-compute-delay 0.15)

;;; Sudoku
;;
(autoload 'sudoku "sudoku" "Start playing sudoku." t)

(setq sudoku-level 'evil)
(setq sudoku-download nil)

;; Enable autoinserter
(setq sudoku-autoinsert-mode t)
(add-hook 'sudoku-after-change-hook 'sudoku-autoinsert)

;;; Version control
(setq vc-follow-symlinks t)

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

    (remove-if-not #'(lambda (x)
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
        ";;; " (file-name-nondirectory (buffer-file-name)) " --- " str "

;; Copyright (C) "
        (substring (current-time-string) -4)
        " by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: " (current-time-string) "
;; Keywords: "
        '(require 'finder)
        ;;'(setq v1 (apply 'vector (mapcar 'car finder-known-keywords)))
        '(setq v1 (mapcar (lambda (x) (list (symbol-name (car x))))
                          finder-known-keywords)
               v2 (mapconcat (lambda (x) (format "%10.0s:  %s" (car x) (cdr x)))
                             finder-known-keywords
                             "\n"))
        ((let ((minibuffer-help-form v2))
           (completing-read "Keyword, C-h: " v1 nil t))
         str ", ") & -2 "

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

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
(setq smtpmail-smtp-user "e.zajcev@corp.mail.ru")

(setq gnus-secondary-select-methods
      '((nnimap "Mail.ru"
                (nnimap-inbox "INBOX")
                (nnimap-server-port 993)
                (nnimap-address "imap.mail.ru")
                (nnimap-stream ssl)
                (nnimap-inbox "INBOX")
                (nnimap-split-methods
                 (("JIRA" "^From.*tasks@jira\\.mail\\.ru")
                  ("target-team" "^Sender:.*target-team[^@]*@ml.corp.mail.ru")
                  ("sentry" "^X-Sentry-Server")
                  ("gitlab" "^X-GitLab-Project")
                  ("unknown" "")))
                )))

(setq gnus-select-method (car gnus-secondary-select-methods))

(setq gnus-logo-color-style 'purp)
(setq gnus-logo-colors
      (cdr (assq gnus-logo-color-style gnus-logo-color-alist)))

(setq user-full-name "Zajcev Evgeny")
(setq user-mail-address "e.zajcev@corp.mail.ru")

;; Allow '/' in group names
(setq gnus-invalid-group-regexp "[: `'\"]\\|^$")


(setq gnus-message-archive-method "nnimap:Mail.ru")
(setq gnus-message-archive-group "Отправленные")

(setq gnus-posting-styles
      '((".*"
         (name "Zajcev Evgeny")
         (address "e.zajcev@corp.mail.ru")
         (signature "lg"))
        ))

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

        ("JIRA" . [simple-query "jira.mail.ru" "http://jira.mail.ru/browse/" ""])
        ("MultiTran" . [simple-query "multitran.ru" "http://www.multitran.ru/c/m.exe?l1=2&l2=1&s=" ""])
        ))

(define-key global-map (kbd "C-c w j") 'webjump)

;;}}}

;;; C-mode
(push (cons 'c-mode "bsd") c-default-style)

;; ERC
(setq erc-track-enable-keybindings nil)

(let ((exwm-debug-on t))
  (load-library "exwmrc"))

;;;;; MAIL.RU stuff

;;; Jenkins
(push "~/.emacs.d/thirdparty/jenkins" load-path)
(autoload 'jenkins "jenkins" "Jenkins CI" t)

(setq jenkins-api-token "ca89eacf6d63db3d95408de4d51c1d3a")
(setq jenkins-url "http://jenkins.trgqa.devmail.ru:80/")
(setq jenkins-username "e.zajcev")
(setq jenkins-viewname "target")

(setq jenkins-colwidth-name 35)

;; Commit messages to git
(defun lg-git-ticket-name ()
  "Extract JIRA ticket name from branch name."
  (let* ((branch (git-symbolic-ref "HEAD"))
         (pbranch (split-string
                   (if (string-match "^refs/heads/" branch)
                       (substring branch (match-end 0))
                     branch)
                   "-")))
    (if (string-equal "trg" (car pbranch))
        (concat (upcase (car pbranch)) "-" (cadr pbranch))
      "")))

(defun lg-add-trg-label ()
  "Insert JIRA ticket name into commit message."
  (let ((ticket (lg-git-ticket-name)))
    (unless (string-empty-p ticket)
      (insert "\n")
      (insert ticket)
      (insert ": "))))

(add-hook 'git-log-edit-mode-hook 'lg-add-trg-label)

;;; Nim langugae
(push "~/.emacs.d/thirdparty/nim-mode" load-path)
;(require 'nim-mode)

;; Enable EXWM
(exwm-enable)

;; For .nix files
(require 'nix-mode)

;;; Mail.ru specific emacs setup
(require 'mailru)

;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (dash undo-tree elpy)))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.mail.ru")
 '(smtpmail-smtp-service 587))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
