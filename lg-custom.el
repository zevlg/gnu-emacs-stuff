(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fd236703d59b15f5fb5c8a109cdf102a7703da164231d199badcf05fe3467748"
     default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(package-selected-packages
   '(@ adaptive-wrap alert all-the-icons auctex auto-complete-clang
                     autopair avy browse-kill-ring ccls chess cider
                     clojure-mode cmake-mode cmake-project
                     color-theme-buffer-local company "company"
                     company-box company-emoji company-lsp
                     company-org-block company-posframe consult
                     counsel coverlay crdt cython-mode dash dashboard
                     desktop-environment devdocs disable-mouse
                     disaster dockerfile-mode dogears dot-mode eev
                     eldev elpy elpygen emojify erlang
                     fill-column-indicator fira-code-mode flycheck
                     flycheck-cython flycheck-pycheckers folding
                     gif-screencast gist gist-org git-gutter
                     git-gutter-fringe+ gitlab gitter gnuplot
                     go-errcheck go-fill-struct go-gen-test go-mode
                     go-snippets golden-ratio-scroll-screen
                     google-translate goto-last-change guess-language
                     haskell-mode helm-company helm-directory
                     helm-exwm helm-git-files helm-git-grep
                     highlight-current-line htmlize ibuffer-vc irony
                     irony-eldoc jedi json-mode keycast
                     language-detection levenshtein link-hint
                     live-py-mode lsp-clangd lsp-docker lsp-mode
                     lsp-python lsp-rust lsp-ui lua-mode magit
                     magit-todos magithub markdown-mode memory-usage
                     meson-mode mini-modeline mlscroll mood-line
                     nhexl-mode nim-mode nimbus-theme openwith
                     org-bullets org-jira org-journal org-ql
                     org-rich-yank org-roam org-superstar origami
                     pabbrev package-lint package-lint-flymake
                     paren-face php-mode poly-org ponylang-mode prism
                     projectile protobuf-mode pycoverage python-mode
                     pyvenv-auto quelpa rainbow-identifiers
                     raindow-identifiers reverse-im rlust rtags rudel
                     rust-mode scad-mode scad-preview selectrum
                     shackle smart-compile smartparens sound-wav
                     stripe-buffer sudo-edit swift-mode swiper symon
                     tracking transient travis undo-tree use-package
                     vertico visual-fill-column w3m wande websocket
                     wgrep wolfram yaml-mode yascroll zig-mode))
 '(paradox-github-token t)
 '(safe-local-variable-values
   '((eval when
           (and (buffer-file-name)
                (not (file-directory-p (buffer-file-name)))
                (string-match-p "^[^.]" (buffer-file-name)))
           (unless (featurep 'package-build)
             (let ((load-path (cons "../package-build" load-path)))
               (require 'package-build)))
           (unless (derived-mode-p 'emacs-lisp-mode)
             (emacs-lisp-mode))
           (package-build-minor-mode)
           (setq-local flycheck-checkers nil)
           (set (make-local-variable 'package-build-working-dir)
                (expand-file-name "../working/"))
           (set (make-local-variable 'package-build-archive-dir)
                (expand-file-name "../packages/"))
           (set (make-local-variable 'package-build-recipes-dir)
                default-directory))
     (checkdoc-minor-mode . t)
     (projectile-project-run-cmd .
                                 "mkdir -p build; cd build; cmake ..; make run")
     (projectile-project-compilation-cmd .
                                         "mkdir -p build; cd build; cmake ..; make")))
 '(send-mail-function 'smtpmail-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
