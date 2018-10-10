;; load path
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
;; (add-to-load-path "elisp" "conf" "public_repos")

;; カスタムファイルを別ファイルにする
(setq custom-file (locate-user-emacs-file "custom.el"))
;; (カスタムファイルが存在しない場合は作成する
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
;; カスタムファイルを読み込む
(load custom-file)

;; @ general

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

;; (unless (eq window-system 'ns)
;;   ;; メニューバーを非表示
;;   (when (fboundp 'menu-bar-mode) (menu-bar-mode 0)))

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format "%f")

;; 行番号表示
;; (when (version<= "26.0.50" emacs-version )
;;   (global-display-line-numbers-mode))

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
(defface my/hl-line-face
  ;; 背景がdarkなら背景色を紺に
  '((((class color) (background dark))
     (:background "NavyBlue" t))
    ;; 背景がlightならば背景色を青に
    (((class color) (background light))
     (:background "LightSkyBlue" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my/hl-line-face)
(global-hl-line-mode t)

;; paren-mode : 対応する括弧を強調して表示する
(setq show-paren-delay 0) ;; 表示までの秒数。初期値は0.125
(show-paren-mode t) ;; 有効化
;; parenのスタイル: expressionは格好内も強調表示
(setq show-paren-style 'expression)
;; フェイスを変更する
(set-face-attribute 'show-paren-match nil
      :background 'unspecified
      :underline "darkgreen")

;; 行末の空白を強調表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; バックアップとオートセーブファイルを~/.emacs.d/backups/へ
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".* " ,(expand-file-name "~/.emacs.d/backups/") t)))

;; 更新されたファイルを自動的に読み込み直す
(global-auto-revert-mode t)

;; Elisp
(add-hook 'emacs-lisp-mode-hook 'my/emacs-lisp-mode-hook)

(defun my/emacs-lisp-mode-hook ()
  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))

;; @keymap

;; C-mにnewline-and-indentを割り当てる
(global-set-key (kbd "C-m") 'newline-and-indent)

;; "C-t"でウィンドウを切り替える
(global-set-key (kbd "C-t") 'other-window)

;; @package

;; initialize
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

;; 何もしないuse-package
(unless (require 'use-package nil t)
  (message "`use-package' is unavailable!  Please install it via `M-x list-packages' if possible.")
  (defmacro use-package (&rest args)))

;; Helm
(use-package helm
  :defer t
  :ensure t
  :init
  (require 'helm-config))

;; AutoComplete
(when (require 'auto-complete-config nil t)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default)
  (setq ac-use-menu-map t)
  (setq ac-ignore-case nil))

;; @lang

;; Web
(when (require 'web-mode nil t)
  ;; 自動的にweb-modeを起動したい拡張子を追加する
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  ;; web-modeのインデント設定用フック
  (defun my/web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2) ; HTMLのインデイント
    (setq web-mode-css-indent-offset 2) ; CSSのインデント
    (setq web-mode-code-indent-offset 2) ; JS, PHP, Rubyなどのインデント
    (setq web-mode-comment-style 2) ; web-mode内のコメントのインデント
    (setq web-mode-style-padding 1) ; <style>内のインデント開始レベル
    (setq web-mode-script-padding 1) ; <script>内のインデント開始レベル
    )
  (add-hook 'web-mode-hook  'my/web-mode-hook)
  )

;; Sass
(use-package sass-mode
  :defer t
  :ensure t
  :mode ("\\.sass\\'"))

;; JavaScript
(use-package js2-mode
  :defer t
  :ensure t
  :hook ((js2-mode . my/js2-mode-hook))
  :mode ("\\.js\\'"))

(defun my/js2-mode-hook ()
  (setq tab-width 2))

;; JSX
(use-package rjsx-mode
  :defer t
  :ensure t
  :hook ((rjsx-mode . my/rjsx-mode-hook))
  :mode ("\\.jsx\\'"))

(defun my/rjsx-mode-hook ()
  (setq tab-width 2))

;; TypeScript
(use-package typescript-mode
  :defer t
  :ensure t
  :hook ((typescript-mode . my/typescript-mode-hook))
  :mode ("\\.ts\\'" . typescript-mode))

(defun my/typescript-mode-hook ()
  (setq tab-width 2))

;; Elm
(use-package elm-mode
  :defer t
  :ensure t
  :hook ((elm-mode . my/elm-mode-hook))
  :mode ("\\.elm\\'" . elm-mode))

(defun my/elm-mode-hook ()
  (setq tab-width 4))
