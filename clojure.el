;; clojure-mode
;(require 'clojure-mode-extra-font-locking)

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'subword-mode)
;(add-hook 'clojure-mode-hook 'yas/minor-mode-on)

;; cider
(require 'cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(setq cider-repl-use-clojure-font-lock t)

;Auto complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(setq ac-delay 0.2)
;(setq ac-use-quick-help t)
(setq ac-quick-help-delay 0.5)
(setq ac-use-fuzzy 1)
;(setq ac-auto-start 1)
;(setq ac-auto-show-menu 1)
(ac-config-default)

;ac-nrepl
(require 'ac-nrepl)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)

(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'cider-mode))
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'cider-repl-mode)) 

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))



