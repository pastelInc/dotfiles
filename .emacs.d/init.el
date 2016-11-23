(require 'package)
(package-initialize)

;; cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(define-key global-map (kbd "\C-x b") 'anything)
