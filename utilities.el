(message "utilities")

(add-to-list 'load-path "~/.emacs.d/utilities")

(require 'window-numbering)
(window-numbering-mode 1)

(require 'windata) ;required by dirtree
(require 'tree-mode) ;required by dirtree
(require 'dirtree)

(require 'transpose-frame)

(require 'better-defaults)

(require 'projectile)
(projectile-global-mode)

