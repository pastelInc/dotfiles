;;; init.el --- pastelInc's .emacs -*- coding: utf-8 ; lexical-binding: t -*-

;;; Commentary:

;; This is pastelInc's init file.

;;; Code:

;;; Editor

;; カスタムファイルを別ファイルにする
(setq custom-file (locate-user-emacs-file "custom.el"))
;; カスタムファイルが存在しない場合は作成する
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
;; カスタムファイルを読み込む
(load custom-file)

;; 文字コード
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; AsciiフォントをConsolasに
;; (set-face-attribute 'default nil
;;                     :family "Consolas"
;;                     :height 120)

;; ツールバー非表示
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))

;; メニューバーを非表示
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))

;; タイトルバーにバッファーのファイルパスを表示
(setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; タブ幅
(setq-default tab-width 4)

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; モードラインに行番号表示
(line-number-mode t)

;; モードラインに列番号表示
(column-number-mode t)

;; ファイルサイズを表示
(size-indication-mode t)

;; 空白を表示
(setq-default show-trailing-whitespace t)

;; dired
(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))

;; hightlight
;; (global-hl-line-mode t)

;; 行末の空白を強調表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; バックアップ
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))

;; オートセーブファイル
(setq auto-save-file-name-transforms
      `((".* " ,(expand-file-name "~/.emacs.d/backups/") t)))

;; 更新されたファイルを自動的に読み込み直す
(global-auto-revert-mode t)

;; フレームの最大化
;; https://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; C-mにnewline-and-indentを割り当てる
(global-set-key (kbd "C-m") 'newline-and-indent)

;; "C-t"でウィンドウを切り替える
(global-set-key (kbd "C-t") 'other-window)

;; ビープ音を消音
(setq ring-bell-function 'ignore)

;;; package
(require 'package) ; package.elを有効化
;; パッケージリポジトリにMarmaladeとMELPAを追加
(add-to-list
 'package-archives
 '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/"))
(package-initialize) ; インストール済みのElispを読み込む
;; 最新のpackageリストを読み込む
(when (not package-archive-contents)
  (package-refresh-contents))

;;; use-package
;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")
  (require 'use-package))

;; paren-mode
(use-package paren
  :custom-face
  (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c"))))
  :custom
  (show-paren-mode t)
  (show-paren-delay 0) ;; 表示までの秒数。初期値は0.125
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;;; eldoc
(use-package eldoc
  :config
  (defun my/emacs-lisp-mode-hook ()
  "Hooks for Emacs Lisp mode."
  (setq eldoc-idle-delay 0.2)
  (setq eldoc-echo-area-use-multiline-p t)
  (turn-on-eldoc-mode))
  (add-hook 'emacs-lisp-mode #'my/emacs-lisp-mode-hook))

;; MacのEmacsでファイル名を正しく扱うための設定
(use-package ucs-normalize
  :config
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)
  ;; Macだけに読み込ませる内容を書く
  :if (eq system-type 'darwin))

;;; overcast-theme
;; (use-package overcast-theme
;;   :ensure t
;;   :config
;;   (load-theme 'overcast))

;;; solarized-theme
;; (use-package solarized-theme
;;   :config
;;   (load-theme 'solarized-light)
;;   :ensure t)

;;; sublime-themes
(use-package sublime-themes
  :config
  (load-theme 'brin t)
  :custom-face
  (company-tooltip ((t (:inherit default :foreground "#c0c5ce" :background "#333"))))
  (company-scrollbar-bg ((t (:background "#333"))))
  (company-scrollbar-fg ((t (:background "deep sky blue"))))
  (company-tooltip-annotation ((t (:foreground "white smoke"))))
  (company-tooltip-annotation-selection ((t (:foreground "black"))))
  (company-tooltip-selection ((t (:foreground "black" :background "deep sky blue"))))
  (company-tooltip-common ((t (:foreground "orange"))))
  (company-tooltip-common-selection ((t (:foreground "black"))))
  :ensure t)

;;; dtrt-indent
(use-package dtrt-indent
  :commands dtrt-indent-mode
  :config
  (dtrt-indent-mode 1)
  :defer 3
  :diminish dtrt-indent-mode
  :ensure t)

;;; Helm
(use-package helm
  :config
  (require 'helm-config)
  :ensure t)

(use-package helm-descbinds
  :after helm
  :config
  (helm-descbinds-mode)
  :ensure t)

;;; Flycheck
(use-package flycheck
  :config
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :ensure t)

;;; Company
(use-package company
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
  :map company-search-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-t" . company-search-toggle-filtering))
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  :defer nil
  :ensure t)

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package company-quickhelp
  :after company
  :ensure t
  :hook
  (company-mode . company-quickhelp-mode)
  :init
  (setq company-quickhelp-color-foreground "#c0c5ce")
  (setq company-quickhelp-color-background "#333"))

;;; projectile
(use-package projectile
  :config
  ;;自動的にプロジェクト管理を開始
  (projectile-mode)
  ;; プロジェクト管理から除外するディレクトリを追加
  (defvar projectile-globally-ignored-directories)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "elm-stuff")
  ;; プロジェクト情報をキャッシュする
  (setq projectile-enable-caching t)
  ;; projectileのプレフィックスキーをs-pに変更
  (defvar projectile-mode-map)
  (define-key projectile-mode-map
    (kbd "s-p") 'projectile-command-map)
  :ensure t)

(use-package helm-projectile
  :after projectile
  :config
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  :ensure t)

;;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; add-node-modules-path
(use-package add-node-modules-path
  :ensure t
  :hook (js2-mode rjsx-mode elm-mode typescript-mode))

;;; Web
(use-package web-mode
  :config
  ;; 自動的にweb-modeを起動したい拡張子を追加する
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  ;; web-modeのインデント設定用フック
  (defun my/web-mode-hook ()
    "Hooks for web mode."
    (setq web-mode-markup-indent-offset 2) ; HTMLのインデイント
    (setq web-mode-css-indent-offset 2) ; CSSのインデント
    (setq web-mode-code-indent-offset 2) ; JS, PHP, Rubyなどのインデント
    (setq web-mode-comment-style 2) ; web-mode内のコメントのインデント
    (setq web-mode-style-padding 1) ; <style>内のインデント開始レベル
    (setq web-mode-script-padding 1) ; <script>内のインデント開始レベル
    )

  (add-hook 'web-mode-hook  'my/web-mode-hook)
  :ensure t)

;;; SCSS
(use-package scss-mode
  :ensure t
  :mode "\\.scss$")

;;; JavaScript
(use-package js2-mode
  :config
  (defun my/js2-mode-hook ()
    "Hooks for js2 mode."
    (setq js-indent-level 2)
    (setq js2-include-browser-externs nil)
    (setq js2-mode-show-parse-errors nil)
    (setq js2-mode-show-strict-warnings nil)
    (setq js2-highlight-external-variables nil)
    (setq js2-include-jslint-globals nil))

  (add-hook 'js2-mode-hook 'my/js2-mode-hook)
  :ensure t
  :mode ("\\.js\\'"))

;;; JSON
(use-package js-mode
  :config
  (defun my/js-mode-hook ()
    "Hooks for js mode."
    (setq js-indent-level 4))

  (add-hook 'js-mode-hook 'my/js-mode-hook)
  :mode ("\\.json\\'"))

;;; React
(use-package rjsx-mode
  :config
  (defun my/rjsx-mode-hook ()
    "Hooks for rjsx mode."
    (setq tab-width 2))

  (add-hook 'rjsx-mode-hook 'my/rjsx-mode-hook)
  :ensure t
  :mode ("\\.jsx\\'"))

;;; TypeScript
(use-package typescript-mode
  :config
  (defun my/typescript-mode-hook ()
    "Hooks for typescript mode."
    (setq typescript-indent-level 2))

  (add-hook 'typescript-mode-hook 'my/typescript-mode-hook)
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode))

;;; Elm
(use-package elm-mode
  :after company
  :config
  (defun my/elm-mode-hook ()
    "Hooks for elm mode."
    (add-to-list 'company-backends 'company-elm))

  (add-hook 'elm-mode-hook 'my/elm-mode-hook)
  :ensure t
  :init
  (setq elm-format-on-save t)
  :mode ("\\.elm\\'" . elm-mode))

(use-package flycheck-elm
  :after flycheck
  :config
  (defun my/flycheck-mode-hook ()
    "Hooks for flycheck mode."
    (flycheck-elm-setup))
  (add-hook 'flycheck-mode-hook 'my/flycheck-mode-hook)
  :ensure t)

;;; Go
(use-package go-mode
  :commands (go-set-project godoc gofmt gofmt-before-save)
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook #'go-set-project)
  :ensure t
  :mode "\\.go\\'")

;;; YAML
(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

;;; Language Server Protocol
(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)
  :ensure t
  :hook ((web-mode scss-mode typescript-mode go-mode python-mode) . lsp))

(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t)

(use-package company-lsp
  :after company
  :config
  (push 'company-lsp company-backends)
  :ensure t)

;;; dimmer
(use-package dimmer
  :config
  (setq dimmer-fraction 0.5)
  (setq dimmer-exclusion-regexp "^\*helm.*\\|^ \*Minibuf-.*\\|^ \*Echo.*")
  (dimmer-mode t)
  :ensure t)

;;; beacon
(use-package beacon
  :config
  (setq beacon-color "green")
  (beacon-mode t)
  :ensure t)

;;; Python
(use-package python-mode
  :config
  (defun my/python-mode-hook ()
    "Hooks for python mode."
    (setq indent-tabs-mode nil)
    (setq indent-level 4)
    (setq python-indent 4))
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  :ensure t
  :mode "\\.py\\'")

(provide 'init)
;;; init.el ends here
