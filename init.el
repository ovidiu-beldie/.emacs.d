(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  ;; use full name for hooks, including the '-hook' suffix
  (setq use-package-hook-name-suffix nil)
  (setq use-package-always-ensure t))

(require 'use-package)

(org-babel-load-file "~/.emacs.d/my_init.org")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(which-key yasnippet avy counsel auto-package-update aggressive-indent aggresive-indent rainbow-mode diminish company ivy doom-modeline use-package org-bullets doom-themes all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
