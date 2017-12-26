(setq gc-cons-threshold 80000000)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq custom-file "~/.emacs-custom.el")
(load custom-file)
(org-babel-load-file "~/.emacs.d/config.org")
; Enables C-x n n to do narrow-to-region
(put 'narrow-to-region 'disabled nil)
(setq gc-cons-threshold 80000)
