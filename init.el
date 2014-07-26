(require 'package)

(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)


;;; List of packages to install
; run M-x eval-buffer
; M-x package-list-packages to view available packages
(defvar my-packages '(better-defaults
                      clojure-mode
                      clojure-test-mode
                      cider
                      rainbow-delimiters
		      popup
		      fuzzy
                      clojure-cheatsheet
                      ;clojure-snippets 
                      clojurescript-mode
                      ;company-mode
                      ;company-cider
                      auto-complete
                      ac-nrepl
                      noctilux-theme
                      zenburn-theme
                      exec-path-from-shell
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(load-theme 'noctilux t)

;;;;;;;;;;;;;;;;;;;
;; Generic settings

;; Use command as the meta key
(setq ns-command-modifier (quote meta))
;; Set alt key to none so it can be used to input special characters
(setq ns-option-modifier (quote none))

;; Don't show startup screen
(setq inhibit-startup-message t)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Prevent audio bell
(setq visible-bell t)

;; Line-wrapping
(set-default 'fill-column 80)

;; Show matching parens
(show-paren-mode t)

;; Enable ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1) 

;; Highlight regions and add special behaviours to regions
;; "C-h d transient" for more info
(setq transient-mark-mode t)

(setq default-directory "~")

;; Enable paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook			  #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook  #'enable-paredit-mode)
(add-hook 'ielm-mode-hook			  #'enable-paredit-mode)
(add-hook 'lisp-mode-hook			  #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook		  #'enable-paredit-mode)
(add-hook 'scheme-mode-hook			  #'enable-paredit-mode)

;;; Position and size
(add-to-list 'default-frame-alist '(left . 0))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; More specific configs
(add-to-list 'load-path "~/.emacs.d")
(load-library "clojure")

;;; Add system path to emacs shell on OSX
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


