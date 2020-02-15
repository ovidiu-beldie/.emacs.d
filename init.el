(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(org-babel-load-file (expand-file-name "~/.emacs.d/my_init.org"))


(use-package doom-modeline)

(use-package cider)

(use-package projectile
  :config (projectile-mode 1)
  :bind ("C-c p" . projectile-command-map))

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
   )







(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(amx-backend (quote auto))
 '(custom-safe-themes
   (quote
    ("72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "845103fcb9b091b0958171653a4413ccfad35552bc39697d448941bcbe5a660d" "a339f231e63aab2a17740e5b3965469e8c0b85eccdfb1f9dbd58a30bdad8562b" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "88049c35e4a6cedd4437ff6b093230b687d8a1fb65408ef17bfcf9b7338734f6" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "1c8171893a9a0ce55cb7706766e57707787962e43330d7b0b6b0754ed5283cda" "001e4dbbdb8d01bb299c0244c489504d51ef5939ace24049079b377294786f7c" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "7aaee3a00f6eb16836f5b28bdccde9e1079654060d26ce4b8f49b56689c51904" default)))
 '(package-selected-packages
   (quote
    (smartparens rainbow-delimiters cider powerline anzu apropospriate-theme darktooth-theme sublime-themes emacs-color-themes ample-theme gruvbox-theme gruvbox material-theme color-theme-sanityinc-solarized doom-themes zenburn-theme color-theme auto-complete which-key try use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
