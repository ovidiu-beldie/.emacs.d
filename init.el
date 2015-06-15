(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
;(add-to-list 'package-archives
;             '("marmalade" . "http://marmalade-repo.org/packages/"))
;(add-to-list 'package-archives
; 	     '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)


;;; List of packages to install
; run M-x eval-buffer
; M-x package-list-packages to view available packages
(defvar my-packages '(
                      package
                      paredit
                      better-defaults
                      clojure-mode
                      ;clojure-test-mode ;- deprecated but cider test doesn't work for my setup
                      cider
                      rainbow-delimiters
		      popup
		      fuzzy
                      clojure-cheatsheet
                      ;clojure-snippets 
                      clojurescript-mode
                      auto-complete
                      ;ac-nrepl ; still needed?
                      ac-cider
                      exec-path-from-shell
                      window-number   ; shortcut to switch to window 
                      projectile      ; remembers project
                      python-mode
                      jedi   ;python
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))



;;;;;;;;;;;;;;;;;;;
;; Generic settings

;; Copy-paste to OSX clipboard
(defun pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

;; key bindings for copy-paste
(global-set-key (kbd "C-c c") 'pbcopy)

(global-set-key (kbd "C-c v") 'pbpaste)
(global-set-key (kbd "C-c x") 'pbcut)


;; Use command as the meta key
(setq ns-command-modifier (quote meta))
;; Set alt key to none so it can be used to input special characters
(setq ns-option-modifier (quote none))

;; Enable mouse support (in terminal mode)
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

(set-default-font "Monaco 10")

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
(load-library "python")

;;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;;; Add system path to emacs shell on OSX
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;;; HACK to fix 'Quit: "empty or unsupported pasteboard type" '
(when (eq window-system 'ns)
  (defadvice ns-get-pasteboard (around hack-empty-pasteboard compile activate)
    (condition-case err
        ad-do-it
      (quit (message "%s" (cadr err))
            nil))))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" "1989847d22966b1403bab8c674354b4a2adf6e03e0ffebe097a6bd8a32be1e19" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "7d4d00a2c2a4bba551fcab9bfd9186abe5bfa986080947c2b99ef0b4081cb2a6" "9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'hickey t)
