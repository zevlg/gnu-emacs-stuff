;; init.el --- Custom configuration for GNU Emacs.
;;
;; Copyright (C) 2015,2016 by Zajcev Evgeny
;;
;; On new host do
;;   M-x package-refresh-contents RET
;;   M-x package-install-selected-packages RET
;; after loading init.el fo the first time
;;
(set-frame-height nil 30)
(require 'package)
;; (add-to-list 'package-archives
;;              '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(push "~/.emacs.d/lisp" load-path)
(push "~/.emacs.d/thirdparty" load-path)
(push "~/dev/gnu-emacs-stuff" load-path)
(push "~/dev/gnu-emacs-stuff/thirdpart" load-path)
(push "~/dev/emacs-stuff" load-path)
(push "~/dev/emacs-stuff/thirdpart" load-path)
(push "/usr/share/emacs24/site-lisp/git" load-path)
(load-library "xemacs-theme-source-code")
(custom-theme-set-faces
 'xemacs
 `(escape-glyph ((t (:weight bold :background "gold" :foreground "blue"
                             :box (:line-width -1 :color "black"))))))

;;
;(set-face-font
; 'default "-xos4-terminus-medium-r-normal--32-320-72-72-c-160-koi8-r")
; (set-face-attribute 'default nil :family "Inconsolata LGC")
; (set-face-attribute 'default nil :height 240)

;; See https://github.com/zevlg/RictyDiminishedL
(set-face-attribute 'default nil :family "RictyDiminishedL")
(set-face-attribute 'default nil :height 300)

(setq inhibit-splash-screen t)
(setq enable-recursive-minibuffers t)
(setq select-enable-primary t)

(setq kill-ring-max 200)

;;; Browse kill ring
;; git clone https://github.com/browse-kill-ring/browse-kill-ring.git
;; Installs `M-y' binding to run `browse-kill-ring' command.
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(setq browse-kill-ring-show-preview nil
      browse-kill-ring-highlight-current-entry t
      browse-kill-ring-highlight-inserted-item t)

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

;;; Uso ido to switch buffers and open files
(setq ido-max-window-height 1)
;; Use `.' for dired
(setq ido-show-dot-for-dired t)
;; Disable confusing auto-merging
(setq ido-auto-merge-work-directories-length -1)
;; Enter directory on `/'
(setq ido-enter-matching-directory 'first)

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
;; remember everything you kill
(setq kill-ring-max 1000)

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

;;{{{ `-- Multitran

;;; Interface to multitran.com
;;
;; https://raw.githubusercontent.com/zevlg/gnu-emacs-stuff/master/multitran.el
;; https://raw.githubusercontent.com/zevlg/emacs-stuff/master/wordfreq.el
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

(setq google-translate-default-source-language "en"
      google-translate-default-target-language "ru")

;;}}}

;;{{{ `-- Different bindings

;;; Remove annoying iconifying
(define-key global-map (kbd "C-z") 'nil)

(define-key global-map (kbd "C-=") 'what-cursor-position)
(define-key global-map (kbd "C-x M-=") 'count-words)

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

(defvar lg-scratch-file (expand-file-name "~/.emacs.d/*scratch-file*"))

(defun lg-switch-to-scratch (arg)
  "Switch to \\*scratch\\* buffer.
If prefix ARG is specified, switch in other window."
  (interactive "P")
  (let ((scbuf (find-file lg-scratch-file)))
    (if arg
        (switch-to-buffer-other-window scbuf)
      (switch-to-buffer scbuf))))

(defun lg-install-switch-to-scratch ()
  (local-set-key (kbd "C-c C-s") 'lg-switch-to-scratch))

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

(electric-indent-mode -1)

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
  "Without prefix ARG run `fixup-whitespace'.
With prefix ARG run `just-one-space'."
  (interactive "P")
  (if arg
      (just-one-space)
    (fixup-whitespace)))

(define-key global-map (kbd "M-\\") 'fixup-whitespace)
(define-key global-map (kbd "C-c <space>") 'fixup-whitespace)

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

;;{{{ `-- Server

(server-start)

;; Bold-ify/red-ify "Server"
(let ((sspec (assq 'server-buffer-clients minor-mode-alist)))
  (setcdr sspec (list (propertize " Server" 'face 'error))))

;;}}}

;;{{{ `-- Grep

;; Do not ask about saving some files
(setq grep-save-buffers nil)

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
               (grep-host-defaults-alist nil))
           ;; Recompute defaults using let-bound values above.
           (grep-compute-defaults)
           (setq template grep-template)))

       (list regexp files-glob template))))

  (let ((grep-template template)
        (grep-find-ignored-files nil))
    (lgrep regexp files-glob default-directory nil)))

;;}}}

;;{{{ `-- Git

(autoload 'git-status "git" "git-status" t)

(defun lg-git-status-install-keys ()
  (local-set-key (kbd "<tab>") 'git-next-file)
  (local-set-key (kbd "<backtab>") 'git-prev-file))

(add-hook 'git-status-mode-hook 'lg-git-status-install-keys)

;; Paste to gist
(defun lg-gist-region (begin end &optional arg)
  "Paste region to gist.
If prefix ARG is specified - create public gist."
  (interactive "r\nP")

  (gist-region begin end (not arg))

  ;; GNU Emacs keeps region active after evaluation, so force
  ;; deactivation
  (deactivate-mark))

;;}}}

;;{{{   `-- C-o - Prefix for -other- commands

;; Make C-o to be keymap for othering
(define-key global-map (kbd "C-O") 'open-line)

(define-key global-map (kbd "C-o") nil)
(define-key global-map (kbd "C-o 0") 'lg-kill-buffer-and-window)
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
(define-key global-map (kbd "C-o k") 'lg-kill-buffer-in-other-window)

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

(defface highlight-current-line-face
  '((t (:background "greenyellow")))
    "Face used to highlight current line."
  :group 'highlight-current-line)

(defun highline-local-on ()
  "Ensure current line is highligted."
  (interactive)
  (unless highlight-current-line-minor-mode
    (highlight-current-line-minor-mode)))

;;; Use highline in several major modes by default
(add-hook 'ibuffer-hooks 'highline-local-on)
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

(setq python-shell-interpreter "python3")

(defun lg-py-shell ()
  "Switch to python interpreter."
  (interactive)
  (unless (python-shell-get-process)
    (run-python))
  (pop-to-buffer
   (process-buffer (python-shell-get-process-or-error))))

(define-key global-map (kbd "C-c d p") 'lg-py-shell)

;; C-ce Editing prefix
(define-key global-map (kbd "C-c e c") 'checkdoc)
(define-key global-map (kbd "C-c e w") 'whitespace-cleanup)
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
(define-key global-map (kbd "C-c m c") 'lg-mini-calc)
(define-key global-map (kbd "C-c m f") 'folding-mode)
(define-key global-map (kbd "C-c m a") 'ascii-display)
(define-key global-map (kbd "C-c m g") 'imenu)
(define-key global-map (kbd "C-c m o") 'occur)
(define-key global-map (kbd "C-c m |") 'vertical-mode)
(define-key global-map (kbd "C-c m v") 'fci-mode)
(define-key global-map (kbd "C-c m h") 'highlight-current-line-minor-mode)
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

;; C-cd Prefix for DICT
(define-key global-map (kbd "C-c d d") 'multitran)
(define-key global-map (kbd "C-c d r") 'multitran)
(define-key global-map (kbd "C-c d t") 'google-translate-at-point)

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
(define-key global-map (kbd "C-c l w") 'whitespace-report)

;;}}}

;;{{{   `-- C-cc - Prefix for Count/Compile commands

(define-key global-map (kbd "C-c c w") 'count-words)
(define-key global-map (kbd "C-c c m") 'count-matches)

(define-key global-map (kbd "C-c c t") 'lg-compile-ctest-target)

;;}}}

;;{{{   `-- C-cg - Git commands

(define-key global-map (kbd "C-x v p") 'vc-push)
(define-key global-map (kbd "C-x v S") 'vc-create-tag)
(define-key global-map (kbd "C-x v s") 'git-status)

(define-key global-map (kbd "C-c g a") 'lg-vc-annotate)
(define-key global-map (kbd "C-c g =") 'vc-diff)
(define-key global-map (kbd "C-c g p") 'vc-push)
(define-key global-map (kbd "C-c g u") 'vc-update)
(define-key global-map (kbd "C-c g s") 'git-status)

(define-key global-map (kbd "C-c g r") 'lg-gist-region)
(define-key global-map (kbd "C-c g l") 'gist-list)

;;}}}

;;{{{ `-- Python

;; Builtint `python-mode' is slow in emacs 25, so use this one
;; https://gitlab.com/python-mode-devs/python-mode.git
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
;(require 'cython-mode)

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
        ))

(define-key global-map (kbd "C-c w j") 'webjump)

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

;;; C-mode
(push (cons 'c-mode "bsd") c-default-style)

;; Make sure rdm/rc/rp are in PATH
(require 'subr-x)                       ; for `string-empty-p'
(require 'rtags)

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
  (c-toggle-electric-state t)

  ;; Nice rtag based eldoc
  (lg-ensure-rdm-is-running)
  (setq eldoc-documentation-function 'rtags-eldoc)

  (local-set-key (kbd "M-.") 'rtags-find-symbol)

  ;; switch .c <--> .h files
  (local-set-key (kbd "C-c C-h") 'ff-find-related-file)

  (local-set-key (kbd "C-c c c") 'lg-compile)
  (local-set-key (kbd "C-c c t") 'lg-compile-ctest-target)
  (local-set-key (kbd "C-c c d") 'lg-disaster) ; inplace disassembler
  (local-set-key (kbd "C-c C-s") 'lg-switch-to-scratch))

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

;;; Nim langugae (package-install 'nim-mode)
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
(pushnew nim-compilation-error-regexp
         compilation-error-regexp-alist-alist)
(pushnew 'nim compilation-error-regexp-alist)

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
  (local-set-key (kbd "C-/") 'lg-diff-revert-hunk))

(add-hook 'diff-mode-hook 'lg-diff-install-keys)

;;; Sh-mode

(add-hook 'sh-mode-hook 'lg-install-switch-to-scratch)

;;; Visible vertical line
(setq fci-rule-column 80
      fci-rule-width 2)
(setq fci-rule-color "gray75")

;;{{{ `-- Desktop

(require 'desktop)

;; Restoring frames does not work under EXWM
(setq desktop-restore-frames nil)

(push 'dired-mode desktop-modes-not-to-save)
(push 'multran-mode desktop-modes-not-to-save)
(push "worklog" desktop-clear-preserve-buffers)
(setq desktop-buffers-not-to-save
      "\\(^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS\\|^worklog\\)$")

(desktop-save-mode 1)
(setq desktop-globals-to-save
      '(desktop-missing-file-warning
        kill-ring-yank-pointer
        kill-ring

        tags-file-name
        tags-table-list
        search-ring
        regexp-search-ring
        register-alist
        file-name-history
        minibuffer-history
        read-expression-history
        read-command-history
        shell-command-history

        netcat-history
        multitran-read-history

        sudoku-custom-puzzles
        sudoku-solved-puzzles
        command-history))

;; Load the desktop
(defvar lg-desktop-last "default")

(defvar lg-desktops
  '(("default" . ".desktop.default")
    ("elisp" . ".desktop.elisp")
    ("home" . ".desktop.home")
    ("exwm" . ".desktop.exwm"))
  "Alist of desktop files.")

(defun lg-desktop-load (&optional name)
  (interactive (list (completing-read
                      "Desktop [default]: "
                      (mapcar #'(lambda (el) (list (car el))) lg-desktops))))
  (when (or (null name) (string= name ""))
    (setq name "default"))
  (let* ((desktop-base-file-name (cdr (assoc name lg-desktops)))
         (desktop-base-lock-name (concat desktop-base-file-name ".lock"))
         (default-directory (expand-file-name user-emacs-directory)))

    (desktop-read user-emacs-directory)
    (message (format "%s desktop loaded" name))))

(defun lg-desktop-save (&optional name)
  (interactive (list (completing-read
                      (format "Desktop [%s]: " lg-desktop-last)
                      (mapcar #'(lambda (el) (list (car el))) lg-desktops))))
  (when (or (null name) (string= name ""))
    (setq name lg-desktop-last))

  (let* ((desktop-base-file-name (cdr (assoc name lg-desktops)))
         (desktop-base-lock-name (concat desktop-base-file-name ".lock")))
    (desktop-save user-emacs-directory t)
    (setq lg-desktop-last name)))

(defun lg-desktop-setup ()
  (mapc #'(lambda (v)
            (desktop-truncate v 50))
        (list search-ring regexp-search-ring minibuffer-history
              shell-command-history command-history)))

(remove-hook 'kill-emacs-hook 'desktop-kill) ; remove default desktop saver
(add-hook 'kill-emacs-hook 'lg-desktop-save) ; save lg desktop on exit
(add-hook 'kill-emacs-hook 'lg-desktop-setup)

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

;;}}}

;; Backing up in single directory
(setq backup-directory-alist
      (list (cons (expand-file-name ".*" "~")
                  (expand-file-name "backups" user-emacs-directory))))

;;; Work specific emacs setup
(ignore-errors
  (require 'work))

;;; Home specific stuff
(ignore-errors
  (require 'home))

(let ((exwm-debug-on t))
  (load-library "exwmrc"))

;; Enable EXWM
(exwm-enable)

;; Load last desktop
(lg-desktop-load)
(message (format "+ %s loaded" user-init-file))

;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (highlight-current-line ein gitlab ponylang-mode pycoverage wolfram circe gist yaml-mode smart-compile rudel folding origami git-gutter-fringe+ google-translate cmake-project coverlay irony-eldoc multitran fill-column-indicator rtags auto-complete-clang disaster haskell-mode autopair nim-mode irony cmake-mode git-gutter dash auctex undo-tree elpy)))
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
