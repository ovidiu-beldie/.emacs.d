(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
 	     '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)


;;; List of packages to install
; run M-x eval-buffer
; M-x package-list-packages to view available packages
(defvar my-packages '(
                      ;dirtree
                      ;tree-mode
		      epl
		      dash
                      paredit
                      better-defaults
                      clojure-mode
                      clojure-test-mode
                      cider
                      rainbow-delimiters
		      popup
		      fuzzy
                      ;clojure-cheatsheet
                      ;clojure-snippets 
                      clojurescript-mode
                      ;company-modeeva
                      ;company-cide
                      auto-complete
                      ac-nrepl
                      ;sublime-themes
                      exec-path-from-shell
                      ;transpose-frame
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))



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
(load-library "utilities")
(load-library "clojure")

;;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;;; Add system path to emacs shell on OSX
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'hickey t)
