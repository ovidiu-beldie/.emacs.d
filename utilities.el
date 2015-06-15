(message "uti")

(add-to-list 'load-path "~/.emacs.d/utilities")

(require 'window-number)
(window-number-meta-mode)

(require 'windata) ;required by dirtree
(require 'tree-mode) ;required by dirtree
(require 'dirtree)

(require 'transpose-frame)

(require 'projectile)
(projectile-global-mode)
