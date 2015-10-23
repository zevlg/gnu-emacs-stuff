;; init.el --- Custom configuration for GNU Emacs.
;;
;; Copyright (C) 2015 by Zajcev Evgeny.
;;

;;(require 'package)
;;(add-to-list 'package-archives
;;             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(package-initialize)
(server-start)

(push "~/.emacs.d/thirdparty" load-path)
(push "~/dev/gnu-emacs-stuff" load-path)
(load-library "xemacs-theme-source-code")

;;
(set-face-font
 'default "-xos4-terminus-medium-r-normal--32-320-72-72-c-160-koi8-r")

(setq inhibit-splash-screen t)
(setq enable-recursive-minibuffers t)

(set-default 'indent-tabs-mode nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(put 'set-goal-column 'disabled nil)

(setq default-scroll-bar-width 6)

(mouse-avoidance-mode 'none)
(blink-cursor-mode 0)
(set-cursor-color "red3")

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

;;}}}

;;{{{ `-- Modeline
(line-number-mode 1)
(column-number-mode 1)

(size-indication-mode 0)                ;file size
(toggle-truncate-lines 1)

(setq display-time-default-load-average nil)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time-mode 1)

(setq line-number-mode t)
(setq column-number-mode t)

(set-face-background 'mode-line "#c0bf8d")
(set-face-attribute 'mode-line nil :height 0.4)

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

  (let* ((chr (logand last-command-event ?\177))
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
(autoload 'highline-local-on "highline" nil t)

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
(define-key emacs-lisp-mode-map (kbd "C-c e e") 'macroexpand-sexp)

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
(define-key global-map (kbd "C-c r g") 'igrep)
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

(setq elpy-modules '(elpy-module-sane-defaults
                     elpy-module-company
                     elpy-module-eldoc
                     elpy-module-flymake
                     elpy-module-highlight-indentation
                     elpy-module-pyvenv
                     elpy-module-yasnippet))

(defun lg-py-install-keys ()
  (local-set-key (kbd "C-c e r") 'py-execute-region)
  (local-set-key (kbd "C-c e b") 'py-execute-buffer)
  (local-set-key (kbd "C-c e f") 'py-execute-def-or-class)
  (local-set-key (kbd "C-c e s") 'py-execute-string)
  (local-set-key (kbd "C-c C-c") 'py-comment-region)
  (local-set-key (kbd "C-j") 'lg-insert-nl-at-eol)
  )

(add-hook 'python-mode-hook 'lg-py-install-keys)

;;; TODO: pyrex mode

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

;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (undo-tree elpy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
