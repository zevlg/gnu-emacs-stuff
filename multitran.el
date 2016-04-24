;;; multitran.el --- Emacs interface to multitran.

;; Copyright (C) 2016 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Apr 13 01:00:05 2016
;; Keywords: dictionary, hypermedia
;; Version: 0.2

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
;; Multitran is 0-dependance Emacs interface to http://multitran.com
;; online dictionary.
;;
;; Multitran supports *tons* of languages, including such languages
;; as: Esperanto, Latin and Luxembourgish.  See http://multitran.com
;; for full list and feel free to add new languages to
;; `multitran-languages-map' if you missing one.
;;
;; Variables to customize:
;; ~~~~~~~~~~~~~~~~~~~~~~
;;
;; * `multitran-languages' - Pair of languages for translation,
;;   available languages are:
;; 
;; * `multitran-header-formatters' - Header line formatters
;;   You might want to add your custom formatters, like:
;; 
;;    (defun my-multitran--hf-wordfreq ()
;;      "Show word's frequency rank."
;;      (let ((wfreq (wordfreq-find (or multitran-word ""))))
;;        (and wfreq (format "FRank: %S" (cadr wfreq)))))
;;
;;    (setq multitran-header-formatters
;;          '(miltitran--hf-word multitran--hf-languages
;;            my-multitran--hf-wordfreq multitran--hf-history))
;;
;;   Where `wordfreq-find' is from
;;   https://raw.githubusercontent.com/zevlg/emacs-stuff/master/wordfreq.el
;; 
;; * `multitran-mode-hook' - hook is run after entering multitran-mode
;; 
;;; History:
;;  ~~~~~~~
;; 
;; Version 0.2:
;;   - Support for header-line-format
;;   - Support for suggestions
;;   - Many languages added
;; 
;; Version 0.1:
;;   - Base port of some rdict functionality
;;   - html parsers
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

(defcustom multitran-header-formatters
  '(miltitran--hf-word multitran--hf-languages multitran--hf-history)
  "*List of format functions to compose multitran header."
  :type 'list
  :group 'multitran)

(defcustom multitran-header-line-format
  '(" " (:eval (multitran--header-line)))
  "*Mode-line-format for multitran buffer.
If non-nil then header is used to display multitran header.
Otherwise header is inserted as plain text on top of multitran buffer."
  :type 'list
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

(defcustom multitran-subject-padding 2
  "*Padding between subject and translation in spaces."
  :type 'number
  :group 'multitran)

(defcustom multitran-fill-column 80
  "*Fill column for multitran buffer."
  :type 'number
  :group 'multitran)

(defcustom multitran-dir (expand-file-name "~/.multitran")
  "*Directory where multitran stores its files."
  :type 'directory
  :group 'multitran)

(defcustom multitran-justify-translate default-justification
  "*Justification for translation part."
  :type (get 'default-justification 'custom-type)
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

    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "]") 'multitran-next-section)
    (define-key map (kbd "[") 'multitran-prev-section)
    (define-key map (kbd "TAB") 'multitran-next-link)
    (define-key map (kbd "<backtab>") 'multitran-prev-link)
    (define-key map (kbd "RET") 'multitran-follow-link)
    (define-key map (kbd "w") 'multitran)

    ;; History navigation
    (define-key map (kbd "h n") 'multitran-history-next)
    (define-key map (kbd "h p") 'multitran-history-prev)
    (define-key map (kbd "h >") 'multitran-history-last)
    (define-key map (kbd "h <") 'multitran-history-first)
    (define-key map (kbd "h l") 'multitran-history-list)

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
(defvar multitran-word "" "Currently translated word.")
(defvar multitran-saved-window-condition nil)

(defconst multitran-url "http://multitran.com"
  "URL to use in order to search for words.")

(defconst multitran-languages-map
  '(("ab" "Abkhazian" "71")
    ("ag" "Adyghe" "72")
    ("af" "Afrikaans" "31")
    ("al" "Albanian" "47")
    ("cn" "Chinese" "17")
    ("cn2" "Chinese Taiwan" "98")
    ("cn3" "Chinese simplified" "97")
    ("cs" "Czech" "16")
    ("en" "English" "1")
    ("eo" "Esperanto" "34")
    ("de" "German" "3")
    ("el" "Greek" "38")
    ("fi" "Finnish" "36")
    ("fr" "French" "4")
    ("ga" "Irish" "49")
    ("it" "Italian" "23")
    ("la" "Latin" "37")
    ("jp" "Japanese" "28")
    ("ko" "Korean" "39")
    ("pt" "Portuguese" "11")
    ("ru" "Russian" "2")
    ("sv" "Swedish" "29")
    ("sk" "Slovak" "60")
    ("sl" "Slovenian" "67")
    ("es" "Spanish" "5")
    ("ua" "Ukranian" "33")
    ))

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

(defface multitran-header-face
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

(defface multitran-section-face
  '((((class color) (background light))
     (:background "Gray70"))
    (((class color) (background dark))
     (:background "Gray70"))
    (((class grayscale) (background light))
     (:background "Gray70"))
    (((class grayscale) (background dark))
     (:background "Gray70")))
  "Face used for displaying translation section."
  :group 'multitran)

(defsubst multitran-lang-code (lang)
  (caddr (assoc (or lang multitran-language) multitran-languages-map)))

(defun multitran-faceify (start end faces)
  (add-face-text-property start end faces))

(defun multitran-insert (text faces)
  (add-face-text-property 0 (length text) faces t text)
  (insert text))

(defun multitran--face-at-point-p (face &optional point)
  "Return non-nil if there is FACE in faces list at POINT."
  (let ((cface (get-text-property (or point (point)) 'face)))
    (or (eq cface 'multitran-section-face)
        (and (listp cface)
             (memq 'multitran-section-face cface)))))

(defun multitran-linkify (start end url)
  "Add link to URL as `multitran-link' property."
  (add-text-properties start end (list 'multitran-link url)))

(defun multitran-link-at (&optional point)
  "Return `multitran-link' property at POINT."
  (get-text-property (or point (point)) 'multitran-link))

(defun miltitran--hf-word ()
  (let ((word (or multitran-word "UNKNOWN")))
    (concat "Word: " (propertize word 'face 'bold))))

(defun multitran--hf-languages ()
  (let ((lang1 (car multitran-languages))
        (lang2 (cdr multitran-languages)))
    (format "%s %c %s" lang1 #x21c4 lang2)))

(defun multitran--hf-history ()
  (format "History: %d/%d"
          multitran-history-index (length multitran-history)))

(defun multitran--header-line ()
  "Return contents for header line."
  (mapconcat
   #'identity (remove-if-not
               #'stringp (mapcar #'funcall multitran-header-formatters))
   ", "))

(defun multitran-mode ()
  "Major mode for browsing multitran output.

Bindings:
\\{multitran-mode-map}"
  (interactive)

  (use-local-map multitran-mode-map)
  (setq major-mode 'multitran-mode
        mode-name "multitran"
        buffer-read-only t
        header-line-format multitran-header-line-format)

  (set-buffer-modified-p nil)

  ;; Finally run hooks
  (run-hooks 'multitran-mode-hook))

(defmacro with-multitran-region (start end &rest body)
  (let ((bufcontent (gensym)))
    `(let ((,bufcontent (buffer-substring ,start ,end)))
       (with-temp-buffer
         (insert ,bufcontent)
         (goto-char (point-min))
         ,@body))))
(put 'with-multitran-region 'lisp-indent-function 2)

(defun multitran--parse-tag (tagname face)
  "Parse <em> tags."
  (save-excursion
    (while (search-forward (concat "<" tagname ">") nil t)
      (replace-match "" nil nil)
      (let ((cpont (point)))
        (search-forward (concat "</" tagname ">") nil t)
        (replace-match "" nil nil)
        (multitran-faceify cpont (point) face)))))

(defun multitran--parse-em ()
  (multitran--parse-tag "em" 'italic))

(defun multitran--parse-i ()
  (multitran--parse-tag "i" 'italic))

(defun multitran--parse-with-replace (what to)
  (save-excursion
    (while (search-forward what nil t)
      (replace-match to nil nil))))

(defun multitran--parse-nbsp ()
  (multitran--parse-with-replace "&nbsp;" " "))

(defun multitran--parse-amp ()
  (multitran--parse-with-replace "&amp;" " "))

(defun multitran--parse-span-small ()
  (save-excursion
    (while (search-forward "<span class=\"small\">" nil t)
      (replace-match "" nil nil)
      (let ((cpont (point)))
        (search-forward "</span>" nil t)
        (replace-match "" nil nil)
        (multitran-faceify cpont (point) 'subscript)))))

(defun multitran--parse-span-gray (&optional rep1 rep2)
  (save-excursion
    (while (search-forward "<span style=\"color:gray\">" nil t)
      (replace-match (or rep1 "") nil nil)
      (let ((cpont (point)))
        (search-forward "</span>" nil t)
        (replace-match (or rep2 "") nil nil)))))

(defun multitran--parse-links (&optional no-props)
  ;; <a href=" -> insert 'multitran-link prop
  (save-excursion
    (while (re-search-forward "<a [^>]*href=[\"']\\([^<>]*\\)[\"']>" nil t)
      (let ((urlstr (match-string 1))
            cpont)

        (replace-match "" nil nil)
        (setq cpont (point))

        (re-search-forward "</[aA]>" nil t)
        (replace-match "" nil nil)

        ;; Fix URL-STR
        (setq urlstr (replace-regexp-in-string "&[aA][mM][pP][;]" "&" urlstr))

        (unless no-props
          (multitran-linkify cpont (point) urlstr)
          (multitran-faceify cpont (point) 'multitran-link-face))))))

(defun multitran--parse-section-title (start end)
  "Extract section's title."
  (with-multitran-region start end
    ;; Remove garbage links to webster, phrases, etc
    (save-excursion
      (when (search-forward "<span style=\"color:gray\">|</span>" nil t)
        (delete-region (match-beginning 0) (point-max))))

    (multitran--parse-links)
    (multitran--parse-span-gray "/" "/")
    (multitran--parse-em)

    (concat (buffer-string) "\n")))

(defconst multitran--section-start "<tr><td colspan=\"2\" class=\"gray\">&nbsp;")

(defun multitran--parse-subj (start end)
  (with-multitran-region start end
    (multitran--parse-links :no-linkfy-and-facefy)
    (buffer-string)))

(defun multitran--parse-trans (start end)
  (with-multitran-region start end
    (multitran--parse-links)
    (multitran--parse-span-gray)
    (multitran--parse-nbsp)
    (multitran--parse-amp)
    (multitran--parse-i)
    (buffer-string)))

(defun multitran--parse-section (start end)
  "Return parsed section.
First element is parsed title, rest elements are in form
\(SUBJ . TRANS\)"
  (let ((sh-start (search-forward "<tr><td colspan=\"2\" class=\"gray\">&nbsp;" end))
        (sec-start (search-forward "</td></tr>" end))
        (sh-end (match-beginning 0))
        section-title subjs-trans)

    (setq section-title (multitran--parse-section-title sh-start sh-end))

    ;; Parse subjects and translations
    (with-multitran-region sec-start end
      (while (search-forward "<tr><td class=\"subj\" width=\"1\">" nil t)
        (let ((subj-start (point))
              (trans-start (search-forward "</td>\n<td class=\"trans\" width=\"100%\">"))
              (subj-end (match-beginning 0))
              (trans-end (and (search-forward "</td></tr>") (match-beginning 0))))
          (push (cons (multitran--parse-subj subj-start subj-end)
                      (multitran--parse-trans trans-start trans-end))
                subjs-trans)
          )))

    (cons section-title (nreverse subjs-trans))))

(defun multitran--parse-html ()
  "Process html contents and return list of sections."
  (goto-char (point-min))
  (let ((start (search-forward "<table width=\"100%\">\n"))
        (end (search-forward "</table>"))
        section-points sections)
    ;; Extract all sections
    (goto-char start)
    (while (search-forward "<tr><td colspan=\"2\" class=\"gray\">&nbsp;" end t)
      (push (match-beginning 0) section-points))
    (push end section-points)

    (while (cdr section-points)
      (let ((s-end (car section-points))
            (s-start (cadr section-points)))
        (goto-char s-start)
        (push (multitran--parse-section s-start s-end) sections)
        (setq section-points (cdr section-points))))

    sections))

(defun multitran--string-to-rectangle (string column &optional justify)
  "Split STRING to insertable rectangle by COLUMN.
Make optional justification by JUSTIFY parameter."
  (with-temp-buffer
    (insert string)
    (let ((fill-column column)
          buffer-lines)
      (fill-paragraph justify)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (push (buffer-substring (point-at-bol) (point-at-eol))
              buffer-lines)
        (forward-line 1))
      (nreverse buffer-lines))))

(defun multitran--insert-section (section subjlen)
  "Insert parsed SECTION with calculated maximum subject length SUBJLEN."
  (multitran-insert (car section) 'multitran-section-face)

  (dolist (subj-trans (cdr section))
    (let ((subj (car subj-trans))
          (trans (cdr subj-trans)))
      ;; Insert subj rightpadding with spaces
      (insert subj)
      (insert (make-string (- subjlen (length subj)) 32))

      ;; Insert translation
      (insert-rectangle
       (multitran--string-to-rectangle
        trans (- multitran-fill-column subjlen) multitran-justify-translate))

       (insert "\n"))))

(defun multitran--try-parse-translation ()
  (let* ((sections (multitran--parse-html))
         (subjects (mapcar #'car (apply #'append (mapcar #'cdr sections))))
         (subjlen (+ multitran-subject-padding
                     (apply #'max (mapcar #'length subjects)))))
    (erase-buffer)

    (save-excursion
      (dolist (section sections)
        (multitran--insert-section section subjlen)))))

(defun multitran--try-parse-suggest ()
  (when (re-search-forward "Suggest: <a href=[^<]+</a>")
    (let ((start (match-beginning 0))
          (end (point)))
      (delete-region end (point-max))
      (delete-region (point-min) start))
    (goto-char (point-min))
    (multitran--parse-links)

    ;; Jump to suggestion link
    (multitran-next-link)))

(defun multitran--url (url)
  "Fetch and view multitran URL."
  (let ((cur-buf (current-buffer)))
    (unless (eq major-mode 'multitran-mode)
      (setq multitran-window-condition (current-window-configuration)))

    (with-current-buffer (get-buffer-create "*multitran*")
      (setq buffer-read-only nil)
      (erase-buffer)

      ;; Extract word from the url
      (when (string-match "s=\\([^&]+\\)" url)
        (setq multitran-word (match-string 1 url)))

      ;; Fetch/process html
      (url-insert-file-contents url)
      (condition-case nil
          (multitran--try-parse-translation)
        (error
         (multitran--try-parse-suggest)))

      ;; Save into history
      (multitran--history-push multitran-word url cur-buf)

      (multitran-mode)
      (current-buffer))))

(defun multitran--word (word)
  (let ((url (format "%s/m.exe?s=%s&l1=%s&l2=%s"
                     multitran-url word
                     (multitran-lang-code (car multitran-languages))
                     (multitran-lang-code (cdr multitran-languages)))))

    (pop-to-buffer (multitran--url url))))

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


;; Navigation
(defun multitran--goto-prev-prop (prop-name)
  "Move point to previous property PROP-NAME."
  ;; TODO:
  ;; previous-single-property-change
  )

(defun multitran--goto-next-prop (prop-name)
  "Move point to next property PROP-NAME."
  (let ((pnt (point))
        (orig-val (get-text-property (point) prop-name))
        (nval nil))
    (while (and pnt (or (null nval) (equal orig-val nval)))
      (setq pnt (next-single-property-change pnt prop-name)
            nval (get-text-property (or pnt (point)) prop-name)))
    (unless pnt
      (signal 'end-of-buffer nil))

    (goto-char pnt)))

(defun multitran-next-link (&optional n)
  "Jump to next N link."
  (interactive "p")
  (dotimes (_ n)
    (multitran--goto-next-prop 'multitran-link)))

(defun multitran-follow-link (relative-url)
  "Follow relative url."
  (interactive (list (multitran-link-at)))

  (when relative-url
    (multitran--url (format "%s%s" multitran-url relative-url))))

(defun multitran--goto-section (direction)
  (let ((found nil))
    (while (not found)
      (if (eq direction :next)
          (next-line)
        (previous-line))
      (goto-char (point-at-bol))

      (setq found (multitran--face-at-point-p 'multitran-section-face)))))

(defun multitran-next-section (&optional n)
  "Jump to next N section."
  (interactive "p")
  (dotimes (_ n)
    (multitran--goto-section :next)))

(defun multitran-prev-section (&optional n)
  "Jump to prev N section."
  (interactive "p")
  (dotimes (_ n)
    (multitran--goto-section :prev)))


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
    (goto-char (point-min))

    (setq multitran-word (plist-get (cdr hi) :word))

    (multitran-mode)))

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
