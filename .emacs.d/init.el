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

;; tempbuf
;; https://www.emacswiki.org/emacs/tempbuf.el
(use-package tempbuf
  :diminish tempbuf-mode
  :ensure nil
  :load-path "site-lisp/tempbuf"
  :init
  (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'find-file-hooks 'turn-on-tempbuf-mode))

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
;; (use-package sublime-themes
;;   :config
;;   (load-theme 'brin t)
;;   :custom-face
;;   (company-tooltip ((t (:inherit default :foreground "#c0c5ce" :background "#333"))))
;;   (company-scrollbar-bg ((t (:background "#333"))))
;;   (company-scrollbar-fg ((t (:background "deep sky blue"))))
;;   (company-tooltip-annotation ((t (:foreground "white smoke"))))
;;   (company-tooltip-annotation-selection ((t (:foreground "black"))))
;;   (company-tooltip-selection ((t (:foreground "black" :background "deep sky blue"))))
;;   (company-tooltip-common ((t (:foreground "orange"))))
;;   (company-tooltip-common-selection ((t (:foreground "black"))))
;;   :ensure t)

;;; doom-themes
(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :config
  (load-theme 'doom-spacegrey t)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  :ensure t)

;; doom-modeline
(use-package doom-modeline
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil)
  :config
  (set-cursor-color "cyan")
  (line-number-mode t)
  (column-number-mode t)
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))
  :ensure t
  :hook
  (after-init . doom-modeline-mode))

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

;;; all-the-icons
(use-package all-the-icons
  :defer t)

;;; Flymake
(use-package posframe
  :ensure t)

(use-package flymake-posframe
  :after posframe
  :custom-face
  (flymake-posframe-foreground-face ((t (:foreground "white"))))
  :hook (flymake-mode . flymake-posframe-mode)
  :load-path "site-lisp/flymake-posframe")

(use-package flymake-diagnostic-at-point
  :after flymake
  :custom
  (flymake-diagnostic-at-point-timer-delay 0.1)
  (flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-popup) ;; or flymake-diagnostic-at-point-display-minibuffer
  :disabled
  :hook
  (flymake-mode . flymake-diagnostic-at-point-mode))

;;; Flycheck
;; (use-package flycheck
;;   :config
;;   ;; disable jshint since we prefer eslint checking
;;   (setq-default flycheck-disabled-checkers
;;                 (append flycheck-disabled-checkers
;;                         '(javascript-jshint)))
;;   (add-hook 'after-init-hook 'global-flycheck-mode)
;;   :ensure t)

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
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 2)
  :defer nil
  :ensure t)

(use-package company-box
  :custom
  (company-box-backends-colors nil)
  (company-box-show-single-candidate t)
  (company-box-max-candidates 20)
  :config
  (with-eval-after-load 'all-the-icons
      (declare-function all-the-icons-faicon 'all-the-icons)
      (declare-function all-the-icons-fileicon 'all-the-icons)
      (declare-function all-the-icons-material 'all-the-icons)
      (declare-function all-the-icons-octicon 'all-the-icons)
      (setq company-box-icons-all-the-icons
            `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.7 :v-adjust -0.15))
              (Text . ,(all-the-icons-faicon "book" :height 0.68 :v-adjust -0.15))
              (Method . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
              (Function . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
              (Constructor . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
              (Field . ,(all-the-icons-faicon "tags" :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
              (Variable . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face))
              (Class . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
              (Interface . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01))
              (Module . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.15))
              (Property . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face)) ;; Golang module
              (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.7 :v-adjust -0.15))
              (Value . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'font-lock-constant-face))
              (Enum . ,(all-the-icons-material "storage" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-orange))
              (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.7 :v-adjust -0.15))
              (Snippet . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))
              (Color . ,(all-the-icons-material "palette" :height 0.7 :v-adjust -0.15))
              (File . ,(all-the-icons-faicon "file-o" :height 0.7 :v-adjust -0.05))
              (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.7 :v-adjust -0.15))
              (Folder . ,(all-the-icons-octicon "file-directory" :height 0.7 :v-adjust -0.05))
              (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-blueb))
              (Constant . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05))
              (Struct . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
              (Event . ,(all-the-icons-faicon "bolt" :height 0.7 :v-adjust -0.05 :face 'all-the-icons-orange))
              (Operator . ,(all-the-icons-fileicon "typedoc" :height 0.65 :v-adjust 0.05))
              (TypeParameter . ,(all-the-icons-faicon "hashtag" :height 0.65 :v-adjust 0.07 :face 'font-lock-const-face))
              (Template . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face)))))
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package company-quickhelp
  :after company
  :custom
  (company-quickhelp-delay 0.8)
  :defines company-quickhelp-delay
  :ensure t
  :hook
  (global-company-mode . company-quickhelp-mode))

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
  :hook (elm-mode typescript-mode web-mode))

;;; Web
(use-package web-mode
  :config
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
  ;; ;; web-modeのインデント設定用フック
  ;; (defun my/web-mode-hook ()
  ;;   "Hooks for web mode."
  ;;   (setq web-mode-markup-indent-offset 2) ; HTMLのインデイント
  ;;   (setq web-mode-css-indent-offset 2) ; CSSのインデント
  ;;   (setq web-mode-code-indent-offset 2) ; JS, PHP, Rubyなどのインデント
  ;;   (setq web-mode-comment-style 2) ; web-mode内のコメントのインデント
  ;;   (setq web-mode-style-padding 1) ; <style>内のインデント開始レベル
  ;;   (setq web-mode-script-padding 1) ; <script>内のインデント開始レベル
  ;;   )

  ;; (add-hook 'web-mode-hook  'my/web-mode-hook)
  (add-to-list 'web-mode-comment-formats '("javascript" . "//"))
  :custom
  (web-mode-markup-indent-offset 2) ; HTMLのインデイント
  (web-mode-css-indent-offset 2) ; CSSのインデント
  (web-mode-code-indent-offset 2) ; JS, PHP, Rubyなどのインデント
  (web-mode-comment-style 2) ; web-mode内のコメントのインデント
  (web-mode-style-padding 1) ; <style>内のインデント開始レベル
  (web-mode-script-padding 1) ; <script>内のインデント開始レベル
  :ensure t)

;;; SCSS
(use-package scss-mode
  :ensure t
  :mode "\\.scss$")

;;; JavaScript
;; (use-package js2-mode
;;   :config
;;   (defun my/js2-mode-hook ()
;;     "Hooks for js2 mode."
;;     (setq js-indent-level 2)
;;     (setq js2-include-browser-externs nil)
;;     (setq js2-mode-show-parse-errors nil)
;;     (setq js2-mode-show-strict-warnings nil)
;;     (setq js2-highlight-external-variables nil)
;;     (setq js2-include-jslint-globals nil))

;;   (add-hook 'js2-mode-hook 'my/js2-mode-hook)
;;   :ensure t
;;   :mode ("\\.js\\'"))

;;; JSON
(use-package json-mode
  :custom (js-indent-level 2)
  :ensure t
  :mode ("\\.json\\'"))

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
  :ensure t
  :init
  (setq elm-format-on-save t)
  :mode ("\\.elm\\'" . elm-mode))

;; (use-package flycheck-elm
;;   :after flycheck
;;   :config
;;   (defun my/flycheck-mode-hook ()
;;     "Hooks for flycheck mode."
;;     (flycheck-elm-setup))
;;   (add-hook 'flycheck-mode-hook 'my/flycheck-mode-hook)
;;   :ensure t)

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
   (require 'lsp-clients)
  :custom
  ;; debug
  (lsp-print-io t)
  (lsp-trace t)
  (lsp-print-performance nil)
  ;; general
  (lsp-auto-guess-root t)
  (lsp-document-sync-method 'incremental) ;; none, full, incremental, or nil
  (lsp-elm-server-install-dir "~/.elm-language-server")
  (lsp-enable-snippet nil)
  (lsp-prefer-flymake t)
  (lsp-response-timeout 10)
  :ensure t
  :hook ((web-mode scss-mode typescript-mode go-mode python-mode elm-mode) . lsp))

(use-package lsp-ui
  :after lsp-mode
  :bind
  (:map lsp-mode-map
        ("C-c C-r" . lsp-ui-peek-find-references)
        ("C-c C-j" . lsp-ui-peek-find-definitions)
        ("C-c i"   . lsp-ui-peek-find-implementation)
        ("C-c m"   . lsp-ui-imenu)
        ("C-c s"   . lsp-ui-sideline-mode)
        ("C-c d"   . my/toggle-lsp-ui-doc))
  :commands lsp-ui-mode
  :custom
  ;; lsp-ui-doc
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature nil)
  (lsp-ui-doc-position 'at-point) ;; top, bottom, or at-point
  (lsp-ui-doc-max-width 120)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit t)
  ;; lsp-ui-flycheck
  (lsp-ui-flycheck-enable nil)
  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-code-actions t)
  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-position 'top)
  ;; lsp-ui-peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
  :ensure t
  :preface
    (defun my/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
         (lsp-ui-doc-mode 1))))

(use-package company-lsp
  :after company
  :config
  (push 'company-lsp company-backends)
  :custom
  (company-lsp-async t)
  (company-lsp-cache-candidates t) ;; auto, t(always using a cache), or nil
  (company-lsp-enable-snippet nil)
  (company-lsp-enable-recompletion t)
  :ensure t)

;;; dimmer
(use-package dimmer
  :config
  (dimmer-mode t)
  :custom
  (dimmer-fraction 0.5)
  (dimmer-exclusion-regexp-list
       '(".*Minibuf.*"
         ".*which-key.*"
         ".*NeoTree.*"
         ".*Messages.*"
         ".*Async.*"
         ".*Warnings.*"
         ".*LV.*"
         ".*Ilist.*"))
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
