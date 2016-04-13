;;; multitran.el --- Emacs interface to multitran.

;; Copyright (C) 2016 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Apr 13 01:00:05 2016
;; Keywords:

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

;;

;;; Code:

(require 'url)

(defgroup multitran nil
  "Interface to the multitran dictionary."
  :prefix "multitran-"
  :group 'hypermedia)

(defvar multitran-language-choices
  '(choice (const :tag "English" "en")
           (const :tag "Deutsche" "de")
           (const :tag "Francaise" "fr")
           (const :tag "Italiano" "it")
           (const :tag "Espanola" "es")))

(defcustom multitran-transcription t
  "*Non-nil to enable transcription for english words.
Transcription is fetched from http://wordreference.com"
  :type 'boolean
  :group 'multitran)

(defcustom multitran-languages '("en" . "ru")
  "*Default languages to translate from and to.
Order does not matter."
  :type `(cons ,multitran-language-choices
               ,multitran-language-choices)
  :group 'multitran)

(defcustom multitran-mode-hook nil
  "*Hook to run before entering multitran-mode."
  :type 'hook
  :group 'multitran)

(defcustom multitran-history-max 100
  "*Maximum words to remember in history."
  :type 'number
  :group 'multitran)

(defcustom multitran-fill-column 80
  "*Fill column for multitran buffer."
  :type 'number
  :group 'multitran)

(defcustom multitran-margin-offset 2
  "*Margin offset to use for different text levels."
  :type 'number
  :group 'multitran)

(defcustom multitran-dir (expand-file-name "~/.multitran")
  "*Directory where multitran stores its files."
  :type 'directory
  :group 'multitran)

(defvar multitran-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "<") 'beginning-of-buffer)
    (define-key map (kbd ">") 'end-of-buffer)
    (define-key map (kbd "s") 'isearch-forward)
    (define-key map (kbd "r") 'isearch-backward)
    (define-key map (kbd "?") 'describe-mode)
    (define-key map (kbd "SPC") 'scroll-up)
    (define-key map (kbd "DEL") 'scroll-down)
    (define-key map (kbd "BS") 'scroll-down)

    (define-key map (kbd "q") 'multitran-restore-windows)
    (define-key map (kbd "]") 'multitran-next)
    (define-key map (kbd "[") 'multitran-prev)
    (define-key map (kbd "TAB") 'multitran-next-link)
    (define-key map (kbd "RET") 'multitran-follow-link)
    (define-key map (kbd "w") 'multitran)

    ;; History navigation
    (define-key map (kbd "h n") 'multitran-hist-next)
    (define-key map (kbd "h p") 'multitran-hist-prev)
    (define-key map (kbd "h >") 'multitran-hist-last)
    (define-key map (kbd "h <") 'multitran-hist-first)
    (define-key map (kbd "h l") 'multitran-hist-list)

    ;; Vocabulary
    (define-key map (kbd "v p") 'multitran-vocab-put)
    (define-key map (kbd "v r") 'multitran-vocab-search-related)
    (define-key map (kbd "v l") 'multitran-vocab-list)
    map)
  "Keymap used in multitran mode.")

(defvar multitran-history nil "List of previous searches.")
(defvar multitran-history-index 0
  "Nth element in `multitran-history' we currently active.")

(defvar multitran-read-history nil "History for `read-string'.")
(defvar multitran-current-word "" "Currently translated word.")
(defvar multitran-saved-window-condition nil)

(defconst multitran-url "http://multitran.com"
  "URL to use in order to search for words.")

(defconst multitran-languages-map
  '(("en" . "1") ("ru" . "2") ("de" . "3")
    ("fr" . "4") ("es" . "5") ("it" . "23")))

(defface multitran-gray-face
  '((((class grayscale) (background light))
     (:background "Gray90"))
    (((class grayscale) (background dark))
     (:foreground "Gray80"))
    (((class color) (background light))
     (:foreground "dimgray"))
    (((class color) (background dark))
     (:foreground "gray"))
    (t (:weight bold :underline t)))
  "Face for gray text."
  :group 'multitran)

(defface multitran-abbrev-face
  '((((class grayscale) (background light))
     (:background "Gray90" :italic t))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :italic t))
    (((class color) (background light))
     (:foreground "brown" :italic t))
    (((class color) (background dark))
     (:foreground "brown4" :italic t))
    (t (:bold t :underline t)))
  "Font lock faces used to highlight abbrevs"
  :group 'multitran)

(defface multitran-link-face
  '((((class grayscale) (background light))
     (:background "Gray90" :italic t :underline t))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :italic t :underline t :bold t))
    (((class color) (background light))
     (:foreground "blue"))
    (((class color) (background dark))
     (:foreground "cyan" :bold t))
    (t (:bold t :underline t)))
  "Font lock face used to highlight links to other words"
  :group 'multitran)

(defface multitran-header-line
  '((((class color) (background light))
     (:foreground "Gray20" :background "Gray90"))
    (((class color) (background dark))
     (:foreground "Gray90" :background "Gray20"))
    (((class grayscale) (background light))
     (:background "LightGray" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "DimGray" :weight bold))
    (t (:weight bold)))
  "Face used for displaying header-line."
  :group 'multitran)

(defsubst multitran-lang-code (lang)
  (cdr (assoc (or lang multitran-language) multitran-languages-map)))

(defun multitran-faceify (start end faces)
  (add-text-properties start end 'face faces))

(defun multitran-insert (text faces)
  (insert (propertize text 'face faces)))

(defun multitran--wordreference-transcription (word)
  "Fetch WORD transcription from http://wordreference.com"
  (and multitran-transcription
       (with-temp-buffer
         (url-insert-file-contents
          (format "http://www.wordreference.com/definition/%s" word))
         (when (re-search-forward "<span id='pronWR' [^>]*>\\([^<]*\\)</span>" nil t)
           (match-string 1)))))

(defun multitran--insert-header ()
  "Insert multitran header."
  (let ((transcription (multitran--wordreference-transcription multitran-current-word)))
    (message "WORD: %s // %s" multitran-current-word transcription)
    (mapcar* #'multitran-insert
             (list "Word: " (or multitran-current-word "UNKNOWN")
                   (if transcription (concat " " transcription) "")
                   (format ", %s %c %s" (car multitran-languages) #x21c4
                           (cdr multitran-languages))
                   (format ", History point: %d/%d"
                           multitran-history-index (length multitran-history)))
             (list '(multitran-header-line)
                   '(multitran-header-line bold)
                   '(multitran-header-line)
                   '(multitran-header-line)
                   '(multitran-header-line)))
    (insert "\n")))

(defun multitran-mode (&optional word)
  "Major mode for browsing multitran output.

Bindings:
\\{multitran-mode-map}"
  (interactive)

  (setq multitran-current-word word)

  ;; Insert multitran header
  (save-excursion
    (goto-char (point-min))
    (multitran--insert-header))

  (use-local-map multitran-mode-map)
  (setq major-mode 'multitran-mode
	mode-name "multitran"
	buffer-read-only t)
  (set-buffer-modified-p nil)

  ;; Remove duplicable extents, they were needed for history.
  ;; (map-extents #'(lambda (ex &rest skip)
  ;;                  (set-extent-property ex 'duplicable nil)))

  ;; Finally run hooks
  (run-hooks 'multitran-mode-hook))

(defun multitran--url (url &optional word)
  "Fetch and view multitran URL."
  (let ((cur-buf (current-buffer)) buf)
    (unless (eq major-mode 'multitran-mode)
      (setq multitran-window-condition (current-window-configuration)))

    (setq buf (get-buffer-create "*multitran*"))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer)

      (url-insert-file-contents url)
;      (multitran--fetch-url url)
;      (multitran--proc-output url)

      ;; Save into history
      (multitran--history-push word url cur-buf)

      (multitran-mode word))
    buf))

(defun multitran--word (word)
  (let ((url (format "%s/m.exe?s=%s&l1=%s&l2=%s"
                     multitran-url word
                     (multitran-lang-code (car multitran-languages))
                     (multitran-lang-code (cdr multitran-languages)))))

    (pop-to-buffer (multitran--url url word))))

;;;###autoload
(defun multitran (word &optional lang)
  "Lookup word in multitran dictionary.
If prefix ARG is given, then select language."
  (interactive
   (list
    (read-string
     (let ((cw (current-word)))
       (if cw (format "Translate word [%s]: " cw) "Translate word: "))
     nil 'multitran-read-history)))

  (when (string= word "")
    (setq word (current-word)))

  (multitran--word word))


;; History
(defun multitran--history-push (word &optional url buf)
  "Push WORD into multitran history."
  ;; truncate history
  (when (> (length multitran-history) multitran-history-max)
    (setq multitran-history (butlast multitran-history)))

  (push (list (buffer-string) :url url :word word :buffer buf) multitran-history)
  (setq multitran-history-index 0))

(defun multitran--history-show ()
  "Show history buffer according to `multitran-history-index' value."
  (let ((hi (nth multitran-history-index multitran-history)))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (car hi))
    (multitran-show-transcription)
    (goto-char (point-min))

    (multitran-mode (plist-get (cdr hi) :word))))

;; If this is called we should be already in the *multitran* buffer.
(defun multitran-history-goto (direction &optional n)
  "Navigate history N times.
DIRECTION is one of 'next or 'prev."
  (interactive)

  (setq n (% n (length multitran-history)))

  ;; Calculate position
  ;; X: 0, 1, 2,...,n		;dir = nil
  ;; Y: n, n-1, n-2,...0	;dir = t
  ;;
  ;; X->Y == Y->X: n-(x|y) where n == length of list minus 1
  (when (eq direction 'next)            ; X -> Y
    (setq multitran-history-index
          (- (length multitran-history) 1 multitran-history-index)))

  (setq multitran-history-index
        (% (+ n multitran-history-index) (length multitran-history))) ;offset

  (when (eq direction 'next)            ; back Y -> X
    (setq multitran-history-index
          (- (length multitran-history) 1 multitran-history-index)))

  (when (= (length multitran-history) 1)
    (error "Only one entry in history"))

  (multitran--history-show))

(defun multitran-history-prev (&optional n)
  "Goto N previous word in history."
  (interactive "p")
  (multitran-history-goto 'prev n))

(defun multitran-history-next (&optional n)
  "Goto N next word in history."
  (interactive "p")
  (multitran-history-goto 'next n))

(defun multitran-history-first ()
  "Goto the first item in multitran history."
  (interactive)
  (setq multitran-history-index (1- (length multitran-history)))
  (multitran--history-show))

(defun multitran-history-last ()
  "Goto the last item in multitran history."
  (interactive)
  (setq multitran-history-index 0)
  (multitran--history-show))

(defun multitran-history-list ()
  "List history items."
  (interactive)
  (with-current-buffer (get-buffer-create "*multitran-history*")
    (erase-buffer)
    (insert (format "%-24s%-32s%s\n" "Word" "URL" "Buffer"))
    (insert (format "%-24s%-32s%s\n" "----" "---" "------"))
    (mapc #'(lambda (he)
              (let ((word (plist-get (cdr he) :word))
                    (url (plist-get (cdr he) :url))
                    (buffer (plist-get (cdr he) :buffer)))
                (insert (format "%-24s%-32S%S\n"
                                (or word "UNKNOWN") url (buffer-name buffer)))))
          multitran-history)
    (set-window-buffer (selected-window) (current-buffer))
    ))

(provide 'multitran)

;;; multitran.el ends here
