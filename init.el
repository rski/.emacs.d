(setq gc-cons-threshold 80000000)
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
      vc-handled-backends nil
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
      default-input-method "greek" ;; C-\ will switch between default-input-method and the actual kb layout
      )

(setq-default tab-width 4
              indent-tabs-mode nil
              show-trailing-whitespace t
              )

(load custom-file)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)
(package-refresh-contents t)
(add-hook 'prog-mode-hook #'bug-reference-prog-mode)

(use-package flycheck
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'after-init-hook #'flycheck-pos-tip-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode)
  (setq flycheck-global-modes '(not emacs-lisp-mode))

  :after evil-leader
  :diminish flycheck-mode
  :config
  (evil-leader/set-key
    "fp" 'flycheck-previous-error
    "fn" 'flycheck-next-error)
  (use-package flycheck-color-mode-line
    :config (setq flycheck-color-mode-line-show-running nil))
  (use-package flycheck-pos-tip))

(use-package flyspell
  :defer t
  :if (executable-find "aspell")
  :init
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :diminish 'flyspell-mode)

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
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

(defun rski/pretty-symbols()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(("lambda" . ?λ)))
  (prettify-symbols-mode))
(add-hook 'emacs-lisp-mode-hook #'rski/pretty-symbols)

(use-package emr
  :commands (emr-initialize)
  :init (add-hook 'c-mode-common-hook (lambda ()
                                        (emr-initialize)
                                        (bind-key "C-c C-a" 'emr-c-insert-include c-mode-base-map))))

(use-package ivy
  :init (ivy-mode)
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

  ;; requires auth tokens to work
  (use-package counsel-spotify
    :defer t
    :after evil-leader
    :init
    (evil-leader/set-key
      "oss" 'counsel-spotify-search-track
      "osa" 'counsel-spotify-search-artist)
    :config
    (defun counsel-spotify-action-linux (action)
      "Version of the function that doesn't shell-quote ACTION, which dbus doesn't seem to like."
      (shell-command (concat counsel-spotify-dbus-call "org.mpris.MediaPlayer2.Player." action)))
    :commands (counsel-spotify-search-album counsel-spotify-search-track counsel-spotify-search-artist))

  (use-package swiper
    :bind ("M-i" . swiper))

  (use-package ivy-xref
    :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

  ;;; This requires the fonts to be installed, M-x all-the-icons-install-fonts
  (use-package all-the-icons-ivy
    :init (all-the-icons-ivy-setup))
  )

(use-package anaconda-mode
  :defer t
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))
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
  :init (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'html-mode-hook 'emmet-mode))

(use-package dockerfile-mode :defer t)

(use-package company-c-headers
  :defer t
  :init (local-backend c-mode-hook company-c-headers))

(use-package yang-mode :defer t
  :bind (:map yang-mode-map
              ("C-c u" . sp-backward-up-sexp)) ;; Take me to your parent. sp is *brilliant*
  :init (add-hook 'yang-mode-hook (lambda ()
                                    (setq imenu-generic-expression
                                          '(("leaf" "leaf \\(.*\\) {" 1)
                                            ("container" "container \\(.*\\) {" 1)
                                            ("list" "list \\(.*\\) {" 1)
                                            ("grouping" "grouping \\(.*\\) {" 1)
                                            ("import" "import \\(.*\\) {" 1)
                                            )))))

(use-package nix-mode :defer t)
(use-package company-nixos-options
  :after  company
  :defer t
  :init (local-backend nixos-mode-hook company-nixos-options))

(unless (getenv "GOPATH")
  (user-error "GOPATH unset"))
(use-package go-mode
  :defer t
  :init (add-hook 'go-mode-hook (lambda () (add-hook 'before-save-hook
                                                (lambda ()
                                                  (gofmt-before-save))
                                                nil t)))
  :config (setq gofmt-command "goimports"
                gofmt-show-errors nil) ;; what do i have flycheck for?
  (evil-define-key 'normal go-mode-map (kbd "gd") 'godef-jump)
  (evil-define-key 'normal go-mode-map (kbd "god") 'godef-jump-other-window)
  (evil-define-key 'normal go-mode-map (kbd "K") 'godoc-at-point)
  (evil-define-key 'visual go-mode-map (kbd "gd") 'godef-jump)
  (evil-define-key 'visual go-mode-map (kbd "god") 'godef-jump-other-window)
  (evil-define-key 'visual go-mode-map (kbd "K") 'godoc-at-point)
  (evil-define-key 'normal godoc-mode-map (kbd "q") 'quit-window)
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

  (use-package go-eldoc :init (add-hook 'go-mode-hook 'go-eldoc-setup))
  (use-package go-guru :init (add-hook 'go-mode-hook 'go-guru-hl-identifier-mode))
  (use-package go-playground :defer t)

  ;;; requires nfs/gocode
  (use-package company-go
    :after company
    :init (local-backend go-mode-hook company-go)
    ;;; this is broken with company-tng
    (setq company-go-insert-arguments nil))

  ;;; requires the gometalinter binary
  (use-package flycheck-gometalinter
    :init (add-hook 'go-mode-hook (lambda () (flycheck-select-checker 'gometalinter)))
    :config
    (setq flycheck-gometalinter-fast t)
    (setq flycheck-gometalinter-disable-linters '("gocyclo" "goconst" "vetshadow"))
    (flycheck-gometalinter-setup))

  (use-package gotest
    :config
    (add-hook 'go-test-mode-hook 'visual-line-mode)
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
      " -args -v=9 " )
    (defun rski/go-current-test-glog-verbose ()
      "Run go test with maximum glog verbosity"
      (interactive)
      ;; let doesn't work but this does so
      (setq go-test-additional-arguments-function #'rski/glog-arg-callback)
      (go-test-current-test)
      (setq go-test-additional-arguments-function nil))

    (defun rski/re-run-go-test ()
      (interactive)
      (with-current-buffer (get-buffer "*Go Test*")
        (recompile)))

    (evil-leader/set-key-for-mode 'go-mode
      "tr" 'rski/re-run-go-test
      "tf" 'go-test-current-file
      "tt" 'go-test-current-test
      "tv" 'rski/go-current-test-glog-verbose)
    (setq go-test-verbose t)) ;; passes -v to go-test so the test names show when running them

  (use-package go-rename
    :init
    (evil-leader/set-key-for-mode 'go-mode
      "rr" 'go-rename)))

(use-package protobuf-mode :defer t)

(use-package rust-mode
  :defer t
  :config
  (use-package flycheck-rust
    :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  (use-package racer
    :init
    (add-hook 'rust-mode-hook 'racer-mode)
    (add-hook 'racer-mode-hook 'eldoc-mode)))

(use-package magit
  :defer t
  :init (setq magit-bury-buffer-function 'magit-mode-quit-window
              magit-log-section-commit-count 20
              ;;; word-wise diffs. This is for the current hunk because all makes magit-status really slow in many big hunks
              magit-diff-refine-hunk 't
              )
  :bind ("C-c g" . magit-status)
  :config
  (set-face-background 'diff-refine-added "green3")
  (defun rski/magit-push-review()
    (interactive)
    (magit-run-git-async "push" "review"))
  (magit-define-popup-action 'magit-push-popup ?g "Push to gerrit" 'rski/magit-push-review)
  (use-package evil-magit
    :config (evil-magit-init)))

(use-package git-gutter
  :defer t
  :config (setq git-gutter:update-interval 0.1)
  ;;; TODO upstream the delete fix
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
  :after evil
  :init (projectile-mode)
  (add-to-list 'projectile-globally-ignored-directories "Godeps")
  :config
  (setq projectile-mode-line
        '(:eval (format " [%s]"
                        (projectile-project-name))))

  (use-package counsel-projectile
    :init (counsel-projectile-mode)
    :bind ("M-I". counsel-projectile-rg))

  (defun rski/c-p-dwim(arg)
    "If inside a project, perform an action, otherwise switch to a project.
    Default action is projectile-switch-to-buffer
    With a numeric argument, the action is projectile-find-file.
    With twice the numeric argument, the action is switch-project"
    (interactive "p")
    (cl-flet ((in-project-action () (cond ((= arg 4) (counsel-projectile-find-file))
                                          ((= arg 16) (counsel-projectile-switch-project))
                                          (t (counsel-projectile-switch-to-buffer)))))
      (if (ignore-errors (projectile-project-root))
          (in-project-action)
        (counsel-projectile-switch-project))))

  (define-key evil-normal-state-map (kbd "C-p") #'rski/c-p-dwim)

  (use-package treemacs :defer t
    :config
    (use-package treemacs-projectile :defer t)))

(use-package helpful
  :defer t
  :config (evil-define-key 'normal helpful-mode-map
            "q" 'quit-window)
  ;; FIXME describe-fuction/variable are broken??
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode)

(use-package ws-butler
  :init
  (add-hook 'text-mode-hook #'ws-butler-mode)
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  :diminish ws-butler-mode)

(use-package hl-todo :init (global-hl-todo-mode))

(use-package anzu
  :diminish anzu-mode
  :init (add-hook 'after-init-hook 'global-anzu-mode))

;;; fonts
(set-face-attribute 'default nil :family "Source Code Pro" :height 105)
(use-package monokai-theme :defer t)
(use-package atom-one-dark-theme :defer t)
(use-package solarized-theme :init (load-theme 'solarized-dark 'no-confirm))

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
  :init (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package org
  :defer t
  :init
  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE" "ABANDONED")))
  (setq org-hide-leading-stars t)
  ;; in long folding buffers, show-paren-mode + display-line-numbers causes the test to move around
  (defun rski/maybe-disable-show-paren-mode ()
    (if (> (point-max) 100)
        (show-paren-mode -1)))
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
    :init (add-hook 'org-mode 'reveal-mode)
    :config
    (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
    (use-package htmlize :defer t)))

(defun rski/indent-buffer()
  "what the fn name says"
  (interactive)
  (indent-region (point-min) (point-max)))

(use-package evil-leader
  :init
  (setq evil-want-integration nil)
  (global-evil-leader-mode)
  :config
  (evil-leader/set-key
    "ee" 'eval-last-sexp
    "xb" 'ivy-switch-buffer
    "xkk" 'kill-current-buffer
    "oo" 'other-window
    "of" 'other-frame
    "ww" 'evil-window-next
    "ws" 'evil-window-split
    "ib" 'rski/indent-buffer
    "," 'execute-extended-command)
  (evil-leader/set-leader ",")

  (use-package evil
    :after evil-leader
    :bind (:map evil-motion-state-map
                (":" . evil-repeat-find-char)
                (";" . evil-ex))
    :config (evil-mode)
    (define-key evil-normal-state-map (kbd "M-.") nil)

    (use-package evil-escape
      :diminish evil-escape-mode
      :config (evil-escape-mode))

    (use-package evil-surround :config(global-evil-surround-mode))

    (use-package evil-collection
      :init
      (setq evil-collection-mode-list
            '(anaconda-mode bookmark calendar comint company compile custom debug diff-mode dired doc-view edebug elfeed elisp-mode elisp-refs eshell eval-sexp-fu eww flycheck ggtags help info ivy macrostep
                            (occur replace)
                            (package-menu package)
                            profiler python racer ruby-mode rtags
                            (term term ansi-term multi-term)
                            woman xref))
      :config (evil-collection-init))))

(use-package undo-tree
  :defer t
  :diminish undo-tree-mode)

(use-package smartparens
  :init
  (add-hook 'prog-mode-hook #'smartparens-mode)
  (add-hook 'text-mode-hook #'smartparens-mode)
  :config
  (evil-leader/set-key
    "sl" 'sp-forward-slurp-sexp
    "sh" 'sp-backward-slurp-sexp
    "su" 'sp-unwrap-sexp
    "sw" 'sp-rewrap-sexp)
  (require 'smartparens-config)
  :diminish smartparens-mode)

(dolist (mode '("eshell" "compilation" "debugger" "shell" "Info" "eww" "elfeed-show"))
  (let ((mode-hook (intern (format "%s-mode-hook" mode))))
    (add-hook mode-hook
              (lambda () (setq show-trailing-whitespace nil)))))

(use-package erc
  :defer t
  :ensure nil
  :init
  (add-hook 'erc-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  :config
  (erc-notifications-mode 1)
  (add-hook 'erc-connect-pre-hook #'erc-spelling-mode)
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
(defun rski/load-config ()
  (interactive)
  (load-file user-init-file))

;;; modeline
(setq-default mode-line-format '("" mode-line-modified mode-line-remote
                                 mode-line-buffer-identification " "
                                 mode-line-position mode-line-modes mode-line-misc-info))
(display-time-mode t)
(display-battery-mode t)
(setq battery-mode-line-format "[%L %b%p%% %t]")

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

(setq gc-cons-threshold 80000)
