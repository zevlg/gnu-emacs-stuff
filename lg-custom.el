(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fd236703d59b15f5fb5c8a109cdf102a7703da164231d199badcf05fe3467748" default))
 '(package-selected-packages
   '(gist-org dogears reverse-im crdt poly-org sudo-edit live-py-mode go-errcheck go-fill-struct go-gen-test go-snippets eglot go-mode protobuf-mode org-bullets levenshtein vertico consult meson-mode mlscroll yascroll projectile w3m disable-mouse link-hint dockerfile-mode tracking @ fira-code-mode golden-ratio-scroll-screen prism selectrum cider paren-face htmlize clojure-mode org-superstar raindow-identifiers chess org-rich-yank org-roam company-posframe openwith quelpa counsel json-mode ess erlang language-detection websocket org-journal memory-usage dashboard avy swiper eldev swift-mode mini-modeline nimbus-theme helm-exwm python-mode dot-mode browse-kill-ring desktop-environment lsp-docker ccls gnuplot symon jedi magithub org-ql gif-screencast php-mode rainbow-identifiers ibuffer-vc mood-line alert package-lint package-lint-flymake helm-git-grep helm-company helm-directory helm-git-files org-jira goto-last-change keycast use-package company-lsp lsp-clangd lsp-mode lsp-python lsp-rust lsp-ui flycheck flycheck-cython flycheck-pycheckers elpygen magit "company" company-emoji emojify sound-wav visual-fill-column pabbrev stripe-buffer all-the-icons travis wande rlust markdown-mode gitter scad-mode scad-preview nhexl-mode rust-mode cython-mode gh smartparens lua-mode highlight-current-line ein gitlab ponylang-mode pycoverage wolfram gist yaml-mode smart-compile rudel folding origami git-gutter-fringe+ google-translate cmake-project coverlay irony-eldoc fill-column-indicator rtags auto-complete-clang disaster haskell-mode autopair nim-mode irony cmake-mode git-gutter dash auctex undo-tree elpy))
 '(paradox-github-token t)
 '(safe-local-variable-values
   '((eval when
           (and
            (buffer-file-name)
            (not
             (file-directory-p
              (buffer-file-name)))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep 'package-build)
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require 'package-build)))
           (unless
               (derived-mode-p 'emacs-lisp-mode)
             (emacs-lisp-mode))
           (package-build-minor-mode)
           (setq-local flycheck-checkers nil)
           (set
            (make-local-variable 'package-build-working-dir)
            (expand-file-name "../working/"))
           (set
            (make-local-variable 'package-build-archive-dir)
            (expand-file-name "../packages/"))
           (set
            (make-local-variable 'package-build-recipes-dir)
            default-directory))
     (checkdoc-minor-mode . t)
     (projectile-project-run-cmd . "mkdir -p build; cd build; cmake ..; make run")
     (projectile-project-compilation-cmd . "mkdir -p build; cd build; cmake ..; make")))
 '(send-mail-function 'smtpmail-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
