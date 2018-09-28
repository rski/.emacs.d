(setq gc-cons-threshold 80000000
      gc-cons-percentage 0.6)
(defvar rski-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;; Overwrite the custom-file clobbering. I think I want this?
(defun package--save-selected-packages (&optional value)
  "Do nothing")

;;; Enables C-x n n to do narrow-to-region
(put 'narrow-to-region 'disabled nil)

;;; Various config options
(setq user-full-name "Romanos Skiadas"
      user-mail-address "rom.skiad@gmail.com"
      custom-file "~/.emacs-custom.el"
      tags-add-tables nil ;;; Don't ask to keep current tags table when changing dirs
      ring-bell-function 'ignore ;;; shut up
      scroll-conservatively 1000 ;;; scroll one line at a time
      sentence-end-double-space nil ;; when filling, use one space after fullstop
      visible-bell nil
      inhibit-startup-screen t
      create-lockfiles nil;; might be a bad idea but for 99% of the time should be ok
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `((".*" . ,(concat user-emacs-directory "backups")))
      auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "backups") t))
      column-number-mode 1
      whitespace-style '(face lines-tail)
      ;; C-\ will switch between default-input-method and the actual kb layout
      default-input-method "greek"
      ;;this makes resizing not trunkate the frame size and create gaps underneath it in awesome
      frame-resize-pixelwise t
      )

;; make C-z do nothing
(defun suspend-frame() (interactive) )

(setq-default tab-width 4
              indent-tabs-mode nil
              show-trailing-whitespace t
              )

;; compose-mail
(global-unset-key (kbd "C-x m"))
;; set-goal-column. Displays that annoying warning
(global-unset-key (kbd "C-x C-n"))

(load custom-file)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)
(add-hook 'prog-mode-hook #'bug-reference-prog-mode)

(use-package flycheck
  :hook ((after-init    . global-flycheck-mode)
         (after-init    . flycheck-pos-tip-mode))
  :init
  (setq flycheck-global-modes '(not emacs-lisp-mode))
  ;;; The manual says doing this doesn't work, but it does?
  ;;; maybe in the :config statnza it won't work
  (setq flycheck-keymap-prefix (kbd "C-c f"))
  :diminish flycheck-mode
  :config
  (use-package flycheck-pos-tip)
  (put 'flycheck-yang-path 'safe-local-variable #'stringp)
  )

(use-package flyspell
  :defer t
  :if (executable-find "aspell")
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :diminish 'flyspell-mode)

(use-package company
  :hook (after-init . global-company-mode)
  :init
  (setq company-minimum-prefix-length 3
        company-idle-delay 0
        company-require-match nil
        company-auto-complete nil
        company-selection-wrap-around t
        company-dabbrev-downcase nil)
  (setq company-backends '(company-semantic
                           company-clang
                           company-cmake
                           company-capf
                           company-files
                           (company-dabbrev-code
                            company-gtags
                            company-etags
                            company-keywords)
                           company-dabbrev))
  :config (company-tng-configure-default)
  (use-package company-statistics
    :config (company-statistics-mode)))

(defmacro local-backend (mode-hook backend)
  "a macro for local company backends
        example: (local-backend python-mode-hook company-anaconda)"
  `(add-hook ',mode-hook
             (lambda ()
               (add-to-list (make-local-variable 'company-backends)
                            ',backend))))

(use-package emr
  :commands (emr-initialize)
  :hook (c-mode-common . (lambda ()
                           (emr-initialize)
                           (bind-key "C-c C-a" 'emr-c-insert-include c-mode-base-map))))

(use-package ivy
  :init (ivy-mode)
  (setq ivy-initial-inputs-alist nil)
  :diminish ivy-mode
  :config (setq ivy-height 20
                ;;; Add recentf and bookmarks to ivy-switch-buffer
                ivy-use-virtual-buffers t)

  ;;; Required for editing search results with ivy-ag and family
  (use-package wgrep :defer t)

  (use-package counsel
    :init (counsel-mode)
    :diminish counsel-mode
    :config
    (setq counsel-yank-pop-height 20
          counsel-yank-pop-separator "\n--\n"
        ;;; Hide files with leading dots. This can be toggled with C-c C-a or by typing a dot
          counsel-find-file-ignore-regexp "\\`\\."
          )
    (use-package smex
      :defer nil)
    :bind (("M-y" . counsel-yank-pop)
           ("M-o" . counsel-semantic-or-imenu)))

  (use-package swiper
    :bind ("M-i" . swiper))

  (use-package ivy-xref
    :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

  ;;; This requires the fonts to be installed, M-x all-the-icons-install-fonts
  ;;; Also note that only 1 transformer can be active, so this effectively prevents me from using say ivy-rich
  (use-package all-the-icons-ivy
    :init (all-the-icons-ivy-setup))
  )

(use-package anaconda-mode
  :defer t
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))
(use-package company-anaconda
  :after company
  :defer t
  :init (local-backend python-mode-hook company-anaconda))

(use-package puppet-mode :defer t)

(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (setq markdown-command "multimarkdown"))

(use-package lua-mode :defer t)
(use-package company-lua
  :defer t
  :after 'company
  :init (local-backend lua-mode-hook company-lua))

(use-package web-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-engines-alist
        '(("django" . "\\.html\\'")))
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-css-colorization t))

(use-package emmet-mode
  :defer t
  :hook ((web-mode . emmet-mode)
         (html-mode . emmet-mode)))

(use-package dockerfile-mode :defer t)

(use-package company-c-headers
  :defer t
  :init (local-backend c-mode-hook company-c-headers))

(use-package yang-mode :defer t
  :bind (:map yang-mode-map
              ("C-c u" . sp-backward-up-sexp)) ;; Take me to your parent. sp is *brilliant*
  :hook (yang-mode . (lambda ()
                       (setq imenu-generic-expression
                             '(("leaf" "leaf \\(.*\\) {" 1)
                               ("container" "container \\(.*\\) {" 1)
                               ("list" "list \\(.*\\) {" 1)
                               ("grouping" "grouping \\(.*\\) {" 1)
                               ("import" "import \\(.*\\) {" 1)
                               )))))

(use-package nix-mode :defer t)
(use-package company-nixos-options
  :after company
  :defer t
  :init (local-backend nixos-mode-hook company-nixos-options))

(unless (getenv "GOPATH")
  (user-error "GOPATH unset"))
(use-package go-mode
  :defer t
  :hook ((go-mode . (lambda () (add-hook 'before-save-hook
                                        (lambda ()
                                          (gofmt-before-save))
                                        nil t)))
         (go-mode . (lambda ()
                      (local-set-key (kbd "M-.") 'godef-jump)
                      (local-set-key (kbd "M-4 M-.") 'godef-jump-other-window))))
  :config (setq gofmt-command "goimports"
                gofmt-show-errors nil) ;; what do i have flycheck for?
  ;; workaround not matching multiline signatures
  ;;  https://github.com/dominikh/go-mode.el/issues/57
  (defun rski/go-mode-setup ()
    (setq-local imenu-generic-expression
                '(("type" "^type *\\([^ \t\n\r\f]*(\\)" 1)
                  ("func" "^func \\(.*\\)(" 1)))
    (setq-local whitespace-line-column 100)
    (whitespace-mode t)
    (setq fill-column 100)
    (auto-fill-mode t))
  (add-hook 'go-mode-hook #'rski/go-mode-setup)

  ;; requires gocode
  (use-package go-eldoc :hook (go-mode . go-eldoc-setup))
  ;; golang.org/x/tools/cmd/guru
  (use-package go-guru :hook (go-mode . go-guru-hl-identifier-mode))
  (use-package go-playground :defer t)

  ;;; requires github.com/mdempsky/gocode (fork of nfs/gocode)
  (use-package company-go
    :after company
    :init (local-backend go-mode-hook company-go)
    ;;; this is broken with company-tng
    (setq company-go-insert-arguments nil))

  ;;; requires the gometalinter binary
  (use-package flycheck-gometalinter
    :if (and (executable-find "gometalinter")
             nil) ;; disable for now
    :hook (go-mode . (lambda () (flycheck-select-checker 'gometalinter)))
    :config
    (setq flycheck-gometalinter-fast t)
    (setq flycheck-gometalinter-disable-linters '("gocyclo" "goconst" "vetshadow"))
    (flycheck-gometalinter-setup))

  (use-package flycheck-golangci-lint
    :hook (go-mode . flycheck-golangci-lint-setup)
    :config (setq flycheck-golangci-lint-tests t
                  flycheck-golangci-lint-fast t)
    )

  (use-package gotest
    :hook (go-test-mode . visual-line-mode)
    :config
    (defvar rski/dlv-debug-last-command nil "")
    (defun rski/dlv-debug-current-test()
      "Get the current test and run it inside dlv"
      (interactive)
      (let ((dlv-buffer (get-buffer "*gud-test*")))
        (if dlv-buffer
            (kill-buffer dlv-buffer)))
      (let ((buff (current-buffer)))
        (switch-to-buffer-other-window buff))
      (let* ((test-name (go-test--get-current-test))
             ;;; TODO something about the -v 9, it breaks packages that don't use glog
             (command (format "dlv test -- -test.run %s -v 9" test-name)))
        (setq rski/dlv-debug-last-command command)
        (message command)
        (dlv command)))
    (defun rski/dlv-debug-last-test()
      "Rerun dlv with the last test debugged."
      (interactive)
      (unless rski/dlv-debug-last-command
        (user-error "It seems this was the first time you tried to debug a test, try running dlv-debug-current-test"))
      (dlv rski/dlv-debug-last-command))

    (defun rski/glog-arg-callback(suite test)
      " -count=1 -args -v=9 " )
    (defun rski/go-current-test-glog-verbose ()
      "Run go test with maximum glog verbosity"
      (interactive)
      ;; let doesn't work but this does so
      (setq go-test-additional-arguments-function #'rski/glog-arg-callback)
      (go-test-current-test)
      (setq go-test-additional-arguments-function nil))

    (defun rski/go-test-current-test ()
      "Run go test -run=CurrrentTest without caching"
      (interactive)
      (let ((current-prefix-arg 1)) ;; emulate C-1
        (call-interactively 'go-test-current-test)))

    (setq go-test-verbose t) ;; passes -v to go-test so the test names show when running them
    :bind (:map go-mode-map
                ("<C-return>" . rski/go-test-current-test))
    )

  ;; requires golang.org/x/tools/cmd/gorename
  (use-package go-rename :defer))

(use-package protobuf-mode :defer t)

(use-package rust-mode
  :defer t
  :config
  (use-package flycheck-rust
    :hook (flycheck-mode . flycheck-rust-setup))
  (use-package racer
    :hook
    ((rust-mode  . racer-mode)
     (racer-mode . eldoc-mode))))

(use-package rpm-spec-mode :defer t)
(use-package yaml-mode :defer t)

(use-package magit
  :defer t
  :init (setq magit-bury-buffer-function 'magit-mode-quit-window
              magit-log-section-commit-count 20
              ;;; word-wise diffs. This is for the current hunk because all makes magit-status really slow in many big hunks
              ;; It is quite distracting so disable it.
              ;; magit-diff-refine-hunk 't
              )
  :bind (("C-c g" . magit-status)
         ("C-c m l" . magit-log-buffer-file)
         ("C-c m b" . magit-blame)
         )
  :config
  (set-face-background 'diff-refine-added "green3")
  (defun rski/magit-push-review()
    (interactive)
    (magit-run-git-async "push" "review"))
  (defun rski/magit-push-review-with-reviewers()
    (interactive)
    (magit-run-git-async "push" "review-with-reviewers"))
  (magit-define-popup-action 'magit-push-popup ?g "Push to gerrit" 'rski/magit-push-review-with-reviewers)
  (magit-define-popup-action 'magit-push-popup ?G "Push to gerrit (no reviewers)" 'rski/magit-push-review)
  )

(use-package git-gutter
  :defer t
  :config (setq git-gutter:update-interval 0.1)
  ;;; TODO upstream the delete fix
  ;; dont fill up /tmp with trash
  (defun git-gutter:live-update ()
    (git-gutter:awhen (git-gutter:base-file)
      (when (and git-gutter:enabled
                 (buffer-modified-p)
                 (git-gutter:should-update-p))
        (let ((file (file-name-nondirectory it))
              (now (make-temp-file "git-gutter-cur"))
              (original (make-temp-file "git-gutter-orig")))
          (when (git-gutter:write-original-content original file)
            (git-gutter:write-current-content now)
            (git-gutter:start-live-update file original now))
          (delete-file now)
          (delete-file original)))))

  :init (global-git-gutter-mode t)
  :diminish git-gutter-mode)

(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  (projectile-mode)
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  :config
  (setq projectile-mode-line
        '(:eval (format " [%s]"
                        (projectile-project-name))))

  (use-package counsel-projectile
    :init (counsel-projectile-mode)
    :bind ("M-I". counsel-projectile-rg))
  )


(use-package helpful
  :defer t
  ;; FIXME describe-fuction/variable are broken??
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode)

(use-package ws-butler
  :hook ((text-mode prog-mode) . ws-butler-mode)
  :diminish ws-butler-mode)

(use-package hl-todo :init (global-hl-todo-mode))

(use-package anzu
  :diminish anzu-mode
  :hook (after-init . global-anzu-mode))

;;; fonts
(set-face-attribute 'default nil :family "Source Code Pro" :height 120)
(use-package monokai-theme :defer t)
(use-package atom-one-dark-theme :defer t)
(use-package solarized-theme :defer t)

;; move it out of the use-packages so that the diffs won't be as weird when I change themes
(load-theme 'monokai 'no-confirm)

(use-package doom-modeline
      :ensure t
      :defer t
      :hook (after-init . doom-modeline-init))

(use-package whitespace
  :defer t
  :ensure nil
  :config
  (set-face-foreground 'whitespace-line "#d33682")
  (set-face-underline 'whitespace-line "#d33682"))

;;; left fringe arrow face (breakpoint triangle)
(defface right-triangle-face
  '((t :foreground "red"))
  "Face for the right-triangle bitmap.")
(set-fringe-bitmap-face 'right-triangle 'right-triangle-face)

(use-package rainbow-delimiters :defer t
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package org
  :defer t
  :init
  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE" "ABANDONED")))
  (setq org-hide-leading-stars t)
  ;; in long folding buffers, show-paren-mode + display-line-numbers causes the test to move around
  (defun rski/maybe-disable-show-paren-mode ()
    (if (> (point-max) 100)
        (setq-local show-paren-mode nil)))
  (add-hook 'org-mode-hook
            #'rski/maybe-disable-show-paren-mode)
  (defun rski/org-mode-hook ()
    (set-fill-column 100)
    (auto-fill-mode t))
  (add-hook 'org-mode-hook #'rski/org-mode-hook)
  :bind (("\C-col" . org-store-link)
         ("\C-coa" . org-agenda)
         ("\C-coc" . org-capture)
         ("\C-cob" . org-switchb))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (octave . t)))
  (setq org-directory (expand-file-name "~/org"))
  (setq org-agenda-files
        '("~/org/todo.org" "~/org/arista.org" "~/org/buy.org" "~/org/daily.org" "~/org/learning.org"))
  (setq org-default-notes-file (concat org-directory "/agenda.org"))
  (setq org-src-fontify-natively t)

  ;;; org-plot/gnuplot requires the gnuplot lib
  (use-package gnuplot :defer t)

  ;;; org reveal (for some reason called ox-reveal too, kinda confusing) for exporting to reveal.js
  (use-package ox-reveal
    :defer t
    :hook (org-mode . (lambda () (load-library "ox-reveal")))
    :config
    (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
    (use-package htmlize :defer t)))

(defun rski/indent-buffer()
  "what the fn name says"
  (interactive)
  (indent-region (point-min) (point-max)))

(use-package smartparens
  :hook ((prog-mode text-mode) . smartparens-mode)
  :config
  (require 'smartparens-config)
  :diminish smartparens-mode)

(dolist (mode '("eshell" "compilation" "debugger" "shell" "Info" "eww" "elfeed-show" "Buffer-menu" "erc"))
  (let ((mode-hook (intern (format "%s-mode-hook" mode))))
    (add-hook mode-hook
              (lambda () (setq show-trailing-whitespace nil)))))

(use-package erc
  :defer t
  :ensure nil
  :hook (erc-connect-pre . erc-spelling-mode)
  :config
  (erc-notifications-mode 1)
  (setq erc-autojoin-mode t)
  (setq erc-pcomplete-nick-postfix ", "))

(use-package elfeed
  :defer t
  :config (setq elfeed-feeds
                '(("http://planet.emacsen.org/atom.xml" emacs)
                  ("http://steve-yegge.blogspot.com/atom.xml" blog emacs)
                  ("http://nullprogram.com/feed/" blog emacs)
                  ("https://jvns.ca/atom.xml" blog)
                  ("https://dave.cheney.net/feed/atom" blog golang)
                  ("https://hackaday.com/blog/feed/" hacks)
                  ("feeds.feedburner.com/Airs-IanLanceTaylor" blog)
                  )))

;;;Rebind M-; to comment out lines instead of insert comments in the end
(global-set-key (kbd "M-;") 'comment-line)

(defun rski/visit-config ()
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c v") 'rski/visit-config)

;;; modeline
(setq-default mode-line-format '("" mode-line-modified mode-line-remote
                                 mode-line-buffer-identification " "
                                 mode-line-position mode-line-modes mode-line-misc-info))
(display-time-mode t)

(use-package eldoc :diminish eldoc-mode :ensure nil)

;;; line numbers
(setq display-line-numbers-grow-only t) ;; confusing otherwise
(global-display-line-numbers-mode 1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(save-place-mode 1)
(show-paren-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-min-dir-content 1))
(global-set-key (kbd "M-/") 'hippie-expand)

;;; double misc after here
(load-file "~/Code/emacs-brewery/brewery.el")

;;; List some unported remacs functions
(defun list-unported-remacs-funcs (remacs-dir)
  (unless (file-directory-p remacs-dir)
    (user-error "dir \"%s\" not found" remacs-dir))
  (let* ((default-directory (concat (file-name-as-directory remacs-dir) "src"))
         (defuns (shell-command-to-string "grep -rnIH \"^DEFUN\""))
         (defun-list (split-string defuns "\n" t)))
    (let ((buff (get-buffer-create "*unported functions*"))
          prev-file)
      (switch-to-buffer-other-window buff)
      (delete-region (point-min) (point-max))
      (org-mode)
      (dolist (line defun-list)
        (let ((current-file (car (split-string line ":" t))))
          (unless (string-equal prev-file current-file)
            (setq prev-file current-file)
            (insert "* " prev-file "\n"))
          (insert "  - " (cadr (split-string line "\"")) "\n")
          )))))

(defun rski/list-unported-emacs-funcs ()
  (interactive)
  (list-unported-remacs-funcs "~/Code/rust/remacs"))

(defun rski/yang-rfc ()
  (interactive)
  (eww "https://tools.ietf.org/html/rfc6020"))

(use-package ediff
  :defer t
  :init
  (defun rski/ediff-wordwise-in-current-buffer ()
    "Thin wrapper around `ediff-regions-wordwise'.
I always end up doing it in current buffer so might as well wrap it."
    (interactive)
    (ediff-regions-wordwise (current-buffer) (current-buffer))))

(use-package comint
  :defer t
  :ensure nil
  :config
  (set-face-attribute 'comint-highlight-input nil :underline "light gray" :weight 'bold)
  (set-face-attribute 'comint-highlight-prompt nil :foreground "deep sky blue"))

(use-package tex-mode
  :defer t
  :ensure nil
  :config (setq tex-dvi-view-command "evince"))

(use-package simple
  :defer t
  ;;; I had wanted to bind these, but magit binds them to go up/down
  ;;; historic commit messages and I do use that. Maybe if I just bind them to prog mode or something.
  ;;; There always is M-g M-n
  ;;; :bind (("M-n" . next-error) ("M-p" . previous-error))
  :ensure nil)

(use-package eshell
  :init (setq eshell-banner-message "Eshell, because the existing shells were not bad enough already.\n\n")
  :hook (eshell-mode . (lambda () (company-mode -1))))

(use-package with-editor
  :hook ((eshell-mode . with-editor-export-editor)
         (eshell-mode . with-editor-export-git-editor)))

(setq battery-echo-area-format "%L %B (%p%% %t)")

(setq gc-cons-threshold 80000
      gc-cons-percentage 0.1
      file-name-handler-alist rski-file-name-handler-alist)
