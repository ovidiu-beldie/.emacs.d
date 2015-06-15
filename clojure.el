;; clojure-mode

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'subword-mode)

;; cider
(require 'cider)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(setq nrepl-log-messages t)
(setq cider-repl-use-clojure-font-lock t)

;Auto complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(setq ac-delay 0.2)
(setq ac-quick-help-delay 0.5)
(setq ac-use-fuzzy 1)
(ac-config-default)


 (require 'ac-cider)
 (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
 (add-hook 'cider-mode-hook 'ac-cider-setup)
 (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
 (eval-after-load "auto-complete"
   '(progn
      (add-to-list 'ac-modes 'cider-mode)
      (add-to-list 'ac-modes 'cider-repl-mode)))

