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
 '(ansi-color-faces-vector
   [default bold shadow italic underline success warning error])
 '(ansi-color-names-vector
   ["#323334" "#C16069" "#A2BF8A" "#ECCC87" "#80A0C2" "#B58DAE" "#86C0D1" "#eceff4"])
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#323232")
 '(column-number-mode t)
 '(custom-safe-themes
   '("7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" default))
 '(fci-rule-color "#525252")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(highlight-tail-colors '(("#2f4a00" . 0) ("#00415e" . 20)))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#cfdf30")
     ("TODO" . "#feacd0")
     ("NEXT" . "#b6a0ff")
     ("THEM" . "#f78fe7")
     ("PROG" . "#00d3d0")
     ("OKAY" . "#4ae8fc")
     ("DONT" . "#80d200")
     ("FAIL" . "#ff8059")
     ("BUG" . "#ff8059")
     ("DONE" . "#44bc44")
     ("NOTE" . "#f0ce43")
     ("KLUDGE" . "#eecc00")
     ("HACK" . "#eecc00")
     ("TEMP" . "#ffcccc")
     ("FIXME" . "#ff9977")
     ("XXX+" . "#f4923b")
     ("REVIEW" . "#6ae4b9")
     ("DEPRECATED" . "#bfd9ff")))
 '(ibuffer-deletion-face 'modus-theme-mark-del)
 '(ibuffer-filter-group-name-face 'modus-theme-mark-symbol)
 '(ibuffer-marked-face 'modus-theme-mark-sel)
 '(ibuffer-title-face 'modus-theme-pseudo-header)
 '(jdee-db-active-breakpoint-face-colors (cons "#000000" "#80A0C2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#000000" "#A2BF8A"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#000000" "#3f3f3f"))
 '(line-number-mode nil)
 '(linum-format " %7i ")
 '(objed-cursor-color "#C16069")
 '(package-selected-packages
   '(lsp-java lsp-treemacs lsp-ivy lsp-ui lsp-mode treemacs-magit treemacs-icons-dired treemacs-projectile treemacs ace-window modus-vivendi-theme modus-operandi-theme modus-vivendi-mode modus-operandi-mode modus-vivendi modus-operandi dimmer sublime-themes company-prescient ivy-prescient projectile magit helpful ivy-rich prog-mode rainbow-delimiters display-line-numbers-mode which-key yasnippet avy counsel auto-package-update aggressive-indent aggresive-indent rainbow-mode diminish company ivy doom-modeline use-package org-bullets doom-themes all-the-icons))
 '(pdf-view-midnight-colors (cons "#eceff4" "#323334"))
 '(rustic-ansi-faces
   ["#323334" "#C16069" "#A2BF8A" "#ECCC87" "#80A0C2" "#B58DAE" "#86C0D1" "#eceff4"])
 '(vc-annotate-background "#323334")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (list
    (cons 20 "#A2BF8A")
    (cons 40 "#bac389")
    (cons 60 "#d3c788")
    (cons 80 "#ECCC87")
    (cons 100 "#e3b57e")
    (cons 120 "#da9e75")
    (cons 140 "#D2876D")
    (cons 160 "#c88982")
    (cons 180 "#be8b98")
    (cons 200 "#B58DAE")
    (cons 220 "#b97e97")
    (cons 240 "#bd6f80")
    (cons 260 "#C16069")
    (cons 280 "#a0575e")
    (cons 300 "#804f54")
    (cons 320 "#5f4749")
    (cons 340 "#525252")
    (cons 360 "#525252")))
 '(vc-annotate-very-old-color nil)
 '(xterm-color-names
   ["#000000" "#ff8059" "#44bc44" "#eecc00" "#2fafff" "#feacd0" "#00d3d0" "#a8a8a8"])
 '(xterm-color-names-bright
   ["#181a20" "#f4923b" "#80d200" "#cfdf30" "#79a8ff" "#f78fe7" "#4ae8fc" "#ffffff"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
