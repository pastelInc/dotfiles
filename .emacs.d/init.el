;; @ general

;; 文字コード
(set-language-environment "Japanese")

;; ツールバー非表示
(tool-bar-mode -1)

;; メニューバーを非表示
(menu-bar-mode -1)

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; 行番号表示
(global-display-line-numbers-mode)

;; 括弧の範囲内を強調表示
(show-paren-mode t)

;; 行末の空白を強調表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; タブ幅
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (elm-mode typescript-mode rjsx-mode js2-mode use-package)))
 '(tab-width 4))

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; モードラインに行番号表示
(line-number-mode t)

;; モードラインに列番号表示
(column-number-mode t)

;; バックアップを残さない
(setq make-backup-files nil)

;; オートセーブを残さない
(setq auto-save-default nil)

;; 空白を表示
(setq-default show-trailing-whitespace t)

;; dired
(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))

;; @package

;; initialize
(require 'package)
(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;; 何もしないuse-package
(unless (require 'use-package nil t)
  (message "`use-package' is unavailable!  Please install it via `M-x list-packages' if possible.")
  (defmacro use-package (&rest args)))

;; JavaScript
(use-package js2-mode
  :defer t
  :ensure t
  :hook ((js2-mode . my/js2-mode-hook))
  :mode
  ("\\.js\\'"))

(defun my/js2-mode-hook ()
  (set (make-local-variable 'tab-width) 2))

;; JSX
(use-package rjsx-mode
  :defer t
  :ensure t
  :hook ((rjsx-mode . my/rjsx-mode-hook))
  :mode
  ("\\.jsx\\'"))

(defun my/rjsx-mode-hook ()
  (set (make-local-variable 'tab-width) 2))

;; TypeScript
(use-package typescript-mode
  :defer t
  :ensure t
  :hook ((typescript-mode . my/typescript-mode-hook))
  :mode ("\\.ts\\'" . typescript-mode))

(defun my/typescript-mode-hook ()
  (set (make-local-variable 'tab-width) 2))

;; Elm
(use-package elm-mode
  :defer t
  :ensure t
  :hook ((elm-mode . my/elm-mode-hook))
  :mode ("\\.elm\\'" . elm-mode))

(defun my/elm-mode-hook ()
  (set (make-local-variable 'tab-width) 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
