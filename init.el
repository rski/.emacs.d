(setq gc-cons-threshold 80000000)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq custom-file "~/.emacs-custom.el")
(load custom-file)

;;; Enables C-x n n to do narrow-to-region
(put 'narrow-to-region 'disabled nil)

(setq user-full-name "Romanos Skiadas"
      user-mail-address "rom.skiad@gmail.com")

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)
(package-refresh-contents t)

(use-package flycheck
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'after-init-hook #'flycheck-pos-tip-mode)
  (add-hook 'flyspell-mode-hook #'flycheck-color-mode-line-mode)
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
  (setq company-backends '(company-css
                           company-semantic
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
  :init (add-hook 'c-mode-common-hook (lambda ()
                                        (emr-initialize)
                                        (bind-key "C-c C-a" 'emr-c-insert-include c-mode-base-map))))

(use-package helm
  :init (helm-mode 1)
  :diminish helm-mode
  :bind (([remap execute-extended-command] . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ([remap find-file] . helm-find-files)
         ("M-o" . 'helm-semantic-or-imenu)
         ([remap switch-to-buffer] . helm-mini))
  :config (setq helm-ff-skip-boring-files t
                helm-buffer-max-length 50)

  (use-package swiper-helm
    :defer t
    :bind (("M-i" . swiper-helm)))

  (use-package helm-tramp :defer t))

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
  :init (add-hook 'before-save-hook 'gofmt-before-save)
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
    (defun rski/glog-arg-callback(suite test)
      " -args -v=9 " )
    (defun rski/go-current-test-glog-verbose ()
      "Run go test with maximum glog verbosity"
      (interactive)
      ;; let doesn't work but this does so
      (setq go-test-additional-arguments-function #'rski/glog-arg-callback)
      (go-test-current-test)
      (setq go-test-additional-arguments-function nil))
    (evil-leader/set-key-for-mode 'go-mode
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
  :init (setq magit-bury-buffer-function 'magit-mode-quit-window)
  :bind ("C-c g" . magit-status)
  :config
  (defun rski/magit-push-review()
    (interactive)
    (magit-run-git-async "push" "review"))
  (magit-define-popup-action 'magit-push-popup ?g "Push to gerrit" 'rski/magit-push-review)
  (use-package evil-magit
    :config (evil-magit-init)))

(setq vc-handled-backends nil)

(use-package git-gutter
  :defer t
  :config (setq git-gutter:update-interval 0.1)
  :init (global-git-gutter-mode t)
  :diminish git-gutter-mode)

(use-package projectile
  :after evil
  :init (projectile-mode)
  (add-to-list 'projectile-globally-ignored-directories "Godeps")
  :config
  (setq projectile-mode-line
        '(:eval (format " Projectile[%s]"
                        (projectile-project-name))))

  ;;;;Helm-ag is required for helm-projectile-ag below
  (use-package helm-ag :defer t)
  (use-package helm-projectile
    :init (helm-projectile-on)
    :bind ("M-I" . helm-projectile-ag))

  (defun rski/c-p-dwim()
    "If inside a project, do helm-mini, otherwise switch to a project."
    (interactive)
    (if (ignore-errors (projectile-project-root))
        (helm-projectile-switch-to-buffer)
      (helm-projectile-switch-project)))
  (define-key evil-normal-state-map (kbd "C-p") #'rski/c-p-dwim)

  (use-package treemacs :defer t
    :config
    (use-package treemacs-projectile :defer t)))

(use-package helpful
  :defer t
  :config (evil-define-key 'normal helpful-mode-map
            "q" 'quit-window)
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode)

(use-package ws-butler
  :defer t
  :init
  (add-hook 'text-mode-hook #'ws-butler-mode)
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  :diminish ws-butler-mode)

(add-hook 'text-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(setq whitespace-style '(face lines-tail))

(use-package hl-todo :init (global-hl-todo-mode))

(use-package anzu
  :diminish anzu-mode
  :init (add-hook 'after-init-hook 'global-anzu-mode))

;;; fonts
(set-face-attribute 'default nil :family "Source Code Pro" :height 105)
(use-package monokai-theme :defer t)
(use-package atom-one-dark-theme :defer t)
(use-package solarized-theme :init (load-theme 'solarized-dark 'no-confirm))

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
  :init (global-evil-leader-mode)
  :config
  (evil-leader/set-key
    "ee" 'eval-last-sexp
    "xb" 'helm-mini
    "xkk" 'kill-current-buffer
    "oo" 'other-window
    "of" 'other-frame
    "ww" 'evil-window-next
    "ws" 'evil-window-split
    "ib" 'rski/indent-buffer
    "," 'execute-extended-command)
  (evil-leader/set-leader ",")

  (use-package evil
    :init (setq evil-want-C-u-scroll t)
    :after evil-leader
    :bind (:map evil-motion-state-map
                (":" . evil-repeat-find-char)
                (";" . evil-ex))
    :config (evil-mode)
    (define-key evil-normal-state-map (kbd "M-.") nil)
    (add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
    (add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)

    (use-package evil-escape
      :diminish evil-escape-mode
      :config (evil-escape-mode))

    (use-package evil-surround :config(global-evil-surround-mode))
    (use-package evil-collection :config (evil-collection-init))))

(use-package undo-tree
  :defer t
  :diminish undo-tree-mode)

(use-package smartparens
  :defer t
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
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))
(use-package erc
  :defer t
  :config
  (add-hook 'erc-mode-hook 'erc-spelling-mode)
  (setq erc-autojoin-mode t)
  (setq erc-pcomplete-nick-postfix ", "))

(use-package elfeed
  :defer t
  :config (setq elfeed-feeds
                '(("http://planet.emacsen.org/atom.xml" emacs)
                  ("http://steve-yegge.blogspot.com/atom.xml" blog emacs)
                  ("http://nullprogram.com/feed/" blog emacs)
                  ("https://jvns.ca/atom.xml" blog))))

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
(setq-default mode-line-format '("" mode-line-modified
                                 mode-line-remote " " mode-line-buffer-identification " "
                                 mode-line-position mode-line-modes mode-line-misc-info))
(display-time-mode t)
(display-battery-mode t)
(setq battery-mode-line-format "[%L %b%p%% %t]")

;;; Don't ask to keep current tags table when changing dirs
(setq tags-add-tables nil)

(use-package eldoc :diminish eldoc-mode)

;;; line numbers
(setq display-line-numbers-grow-only t) ;; confusing otherwise
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;;; shut up
(setq ring-bell-function 'ignore)

;;; scroll one line at a time
(setq scroll-conservatively 1000)

(setq sentence-end-double-space nil) ;; when filling, use one space after fullstop
(defalias 'yes-or-no-p 'y-or-n-p)
(setq column-number-mode 1)
(setq backup-directory-alist
      `((".*" . "~/.tmp/emacs")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.tmp/emacs" t)))
(setq visible-bell nil)
(setq inhibit-startup-screen t)
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
(add-hook 'prog-mode-hook (lambda () (setq tab-width 4))) ; 8 is the default and that is waaaay to much
(setq create-lockfiles nil);; might be a bad idea but for 99% of the time should be ok

;;; inlined from better-defaults, minus the cruft
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-min-dir-content 1)
(require 'saveplace)
(setq-default save-place t)
(global-set-key (kbd "M-/") 'hippie-expand)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;;; double misc after here
(use-package brewery
  :defer t
  :commands dlv-debug-current-test ;; Whenever I open a go file, this function gets evaluated, pulling the rest of the file along with other commands that should be autoloaded but aren't, so they become available by chance. Eh.
  :ensure nil
  :load-path "~/Code/emacs-brewery/")

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

(defun rski/rfc (rfc)
  (interactive "nView RFC>")
  (eww (format "https://tools.ietf.org/html/rfc%s" rfc)))


(setq gc-cons-threshold 80000)
