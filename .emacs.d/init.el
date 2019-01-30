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

;; Macだけに読み込ませる内容を書く
(when (eq system-type 'darwin)
  ;; MacのEmacsでファイル名を正しく扱うための設定
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

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

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format "%f")

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
  (defvar dired-use-ls-dired nil))

;; hightlight
;; (global-hl-line-mode t)

;; paren-mode
(defvar show-paren-delay 0) ;; 表示までの秒数。初期値は0.125
(show-paren-mode t)
;; parenのスタイル: expressionは格好内も強調表示
(defvar show-paren-style 'expression)
;; フェイスを変更する
(set-face-attribute 'show-paren-match nil
      :background 'unspecified
      :underline "darkgreen")

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

;;; Emacs Lisp
(defun my/emacs-lisp-mode-hook ()
  "Hooks for Emacs Lisp mode."
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))

(add-hook 'emacs-lisp-mode-hook 'my/emacs-lisp-mode-hook)

;;; nova-theme
(use-package nova-theme
  :ensure t
  :init
  (load-theme 'nova t))

;; reference to https://stackoverflow.com/questions/19362373/font-color-of-mode-line-becomes-black-in-emacsclient
(defun my/reload-theme (&rest _)
  "Reload theme when a frame is created."
  (when (display-graphic-p)
    (mapc 'disable-theme custom-enabled-themes)
    (load-theme 'nova t)))

(advice-add 'make-frame :after #'my/reload-theme)

;;; dtrt-indent
(use-package dtrt-indent
  :ensure t
  :commands dtrt-indent-mode
  :diminish dtrt-indent-mode
  :defer 3
  :config
  (dtrt-indent-mode 1))

;;; Helm
(use-package helm
  :defer t
  :ensure t
  :config
  (require 'helm-config))

(use-package helm-descbinds
  :defer t
  :ensure t
  :after helm
  :config
  (helm-descbinds-mode))

;;; Auto Complete
(when (require 'auto-complete-config nil t)
  (defvar ac-mode-map)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (define-key ac-mode-map (kbd "C-n") 'ac-next)
  (define-key ac-mode-map (kbd "C-p") 'ac-previous)
  (defvar ac-use-menu-map t)
  (defvar ac-ignore-case nil))

;;; Flycheck
(use-package flycheck
  :defer t
  :ensure t
  :init
  (global-flycheck-mode)
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint))))

(use-package flycheck-elm
  :defer t
  :ensure t
  :after flycheck
  :config
  (defun my/flycheck-mode-hook ()
    "Hooks for flycheck mode."
    (flycheck-elm-setup))

  (add-hook 'flycheck-mode-hook 'my/flycheck-mode-hook))

;;; Company
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (add-hook 'after-init-hook 'company-quickhelp-mode))

;;; projectile
(when (require 'projectile nil t)
  ;;自動的にプロジェクト管理を開始
  (projectile-mode)
  ;; プロジェクト管理から除外するディレクトリを追加
  (defvar projectile-globally-ignored-directories)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "elm-stuff")
  ;; プロジェクト情報をキャッシュする
  (defvar projectile-enable-caching t)
  ;; projectileのプレフィックスキーをs-pに変更
  (defvar projectile-mode-map)
  (define-key projectile-mode-map
    (kbd "s-p") 'projectile-command-map))

(when (require 'helm-projectile nil t)
  (defvar projectile-completion-system 'helm))

;;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; add-node-modules-path
(use-package add-node-modules-path
  :ensure t
  :hook ((js2-mode . add-node-modules-path)
         (rjsx-mode . add-node-modules-path)))

;;; Web
(when (require 'web-mode nil t)
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
    (defvar web-mode-markup-indent-offset 2) ; HTMLのインデイント
    (defvar web-mode-css-indent-offset 2) ; CSSのインデント
    (defvar web-mode-code-indent-offset 2) ; JS, PHP, Rubyなどのインデント
    (defvar web-mode-comment-style 2) ; web-mode内のコメントのインデント
    (defvar web-mode-style-padding 1) ; <style>内のインデント開始レベル
    (defvar web-mode-script-padding 1) ; <script>内のインデント開始レベル
    )

  (add-hook 'web-mode-hook  'my/web-mode-hook))

;;; SCSS
(use-package sass-mode
  :defer t
  :ensure t
  :mode ("\\.sass\\'"))

;;; JavaScript
(use-package js2-mode
  :defer t
  :ensure t
  :mode ("\\.js\\'")
  :config
  (defun my/js2-mode-hook ()
    "Hooks for js2 mode."
    (setq js-indent-level 2)
    (setq js2-include-browser-externs nil)
    (setq js2-mode-show-parse-errors nil)
    (setq js2-mode-show-strict-warnings nil)
    (setq js2-highlight-external-variables nil)
    (setq js2-include-jslint-globals nil))

  (add-hook 'js2-mode-hook 'my/js2-mode-hook))

;;; JSON
(use-package js-mode
  :defer t
  :mode ("\\.json\\'")
  :config
  (defun my/js-mode-hook ()
    "Hooks for js mode."
    (setq js-indent-level 4))

  (add-hook 'js-mode-hook 'my/js-mode-hook))

;;; React
(use-package rjsx-mode
  :defer t
  :ensure t
  :mode ("\\.jsx\\'")
  :config
  (defun my/rjsx-mode-hook ()
    "Hooks for rjsx mode."
    (setq tab-width 2))

  (add-hook 'rjsx-mode-hook 'my/rjsx-mode-hook))

;;; TypeScript
(use-package typescript-mode
  :defer t
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode)
  :config
  (defun my/typescript-mode-hook ()
    "Hooks for typescript mode."
    (setq typescript-indent-level 2))

  (add-hook 'typescript-mode-hook 'my/typescript-mode-hook))

;;; Elm
(use-package elm-mode
  :ensure t
  :after company
  :mode ("\\.elm\\'" . elm-mode)
  :config
  (defun my/elm-mode-hook ()
    "Hooks for elm mode."
    (add-to-list 'company-backends 'company-elm)
    (setq elm-format-on-save t))

  (add-hook 'elm-mode-hook 'my/elm-mode-hook))

;;; Go
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :commands (go-set-project godoc gofmt gofmt-before-save)
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'go-set-project))

;;; YAML
(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

;;; Language Server Protocol
(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil))
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package company-lsp
  :ensure t
  :after company
  :commands company-lsp)

(provide 'init)
;;; init.el ends here
