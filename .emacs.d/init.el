(require 'package)
(package-initialize)

;; cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; view
(column-number-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(global-linum-mode t)
(set-face-background 'trailing-whitespace "#b14770")
(set-frame-parameter (selected-frame) 'alpha '(0.87))
;(set-scroll-bar-mode nil)
(show-paren-mode t)
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))
(setq inhibit-startup-message t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(setq scroll-conservatively 1)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil);
(setq-default show-trailing-whitespace t)
(setq initial-scratch-message "")
(size-indication-mode t)
;(tool-bar-mode -1)

;; encoding
(set-language-environment "Japanese")
(let ((ws window-system))
  (cond ((eq ws 'w32)
         (prefer-coding-system 'utf-8-unix)
         (set-default-coding-systems 'utf-8-unix)
         (setq file-name-coding-system 'sjis)
         (setq locale-coding-system 'utf-8))
        ((eq ws 'ns)
         (require 'ucs-normalize)
         (prefer-coding-system 'utf-8-hfs)
         (setq file-name-coding-system 'utf-8-hfs)
         (setq locale-coding-system 'utf-8-hfs))))
(let ((ws window-system))
  (cond ((eq ws 'w32)
         (set-face-attribute 'default nil
                             :family "Consolas"
                             :height 100)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Meiryo")))
        ((eq ws 'ns)
         (set-face-attribute 'default nil
                             :family "Monaco"
                             :height 140)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Monaco")))))

;; key
(define-key global-map (kbd "\C-x b") 'anything)
