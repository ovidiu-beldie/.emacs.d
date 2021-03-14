;;; init.el -- emacs init file
;;; Commentary: most of the configuration is placed in my_init.org

;;; Code

(require 'package)

(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 15)
	("melpa" . 10)
	("gnu" . 5)))


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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#1B2229" "#BF616A" "#A3BE8C" "#ECBE7B" "#8FA1B3" "#c678dd" "#46D9FF" "#DFDFDF"])
 '(custom-safe-themes
   '("a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" default))
 '(fci-rule-color "#56697A")
 '(jdee-db-active-breakpoint-face-colors (cons "#10151C" "#5EC4FF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#10151C" "#8BD49C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#10151C" "#384551"))
 '(linum-format " %7i ")
 '(package-selected-packages
   '(company-terraform terraform-mode vterm multiple-cursors expand-region git-gutter+ lsp-java lsp-metals sbt-mode scala-mode cider flycheck company-lsp lsp-treemacs lsp-ivy lsp-ui lsp-mode lispy rainbow-delimiters aggressive-indent helpful diminish company-box modus-vivendi-theme modus-operandi-theme doom-modeline sublime-themes doom-themes all-the-icons org-bullets origami treemacs-magit treemacs-icons-dired treemacs-projectile treemacs ace-window avy which-key yasnippet-snippets yasnippet company counsel auto-package-update exec-path-from-shell use-package))
 '(vc-annotate-background "#2F3841")
 '(vc-annotate-color-map
   (list
    (cons 20 "#A3BE8C")
    (cons 40 "#bbbe86")
    (cons 60 "#d3be80")
    (cons 80 "#ECBE7B")
    (cons 100 "#e2ab77")
    (cons 120 "#d99973")
    (cons 140 "#D08770")
    (cons 160 "#cc8294")
    (cons 180 "#c97db8")
    (cons 200 "#c678dd")
    (cons 220 "#c370b6")
    (cons 240 "#c16890")
    (cons 260 "#BF616A")
    (cons 280 "#a35f69")
    (cons 300 "#875e68")
    (cons 320 "#6b5c67")
    (cons 340 "#65737E")
    (cons 360 "#65737E")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
(put 'downcase-region 'disabled nil)
