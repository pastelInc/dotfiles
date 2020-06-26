;;; init.el --- pastelInc's configration -*- coding: utf-8 ; lexical-binding: t -*-

;;; Commentary:

;; This is pastelInc's init file.

;;; Code:

;;;; Load Path

;; カスタムファイルを別ファイルにする
(setq custom-file (locate-user-emacs-file "custom.el"))
;; カスタムファイルが存在しない場合は作成する
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
;; カスタムファイルを読み込む
(load custom-file)

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

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")
  (require 'use-package))

;; バックアップ
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))

;; オートセーブファイル
(setq auto-save-file-name-transforms
      `((".* " ,(expand-file-name "~/.emacs.d/backups/") t)))

;;;; Generic

;;;;; Editing

;; Default Encoding
;; reference to https://stackoverflow.com/questions/2901541/which-coding-system-should-i-use-in-emacs
(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

;; タイトルバーにバッファーのファイルパスを表示
(setq frame-title-format nil)

;; ビープ音を消音
(setq ring-bell-function 'ignore)

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; ツールバー非表示
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))

;; メニューバーを非表示
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; タブ幅
(setq-default tab-width 4)

;; 空白を表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; モードラインに行番号表示
(line-number-mode t)

;; モードラインに列番号表示
(column-number-mode t)

;; ファイルサイズを表示
(size-indication-mode nil)

;; 更新されたファイルを自動的に読み込み直す
(global-auto-revert-mode t)

;; フレームの最大化
;; https://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; ファイル末尾に改行文字を自動挿入しない
(setq require-final-newline nil)

;;;;; paren
(use-package paren
  :custom-face
  (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c"))))
  :custom
  (show-paren-mode t)
  (show-paren-delay 0) ; 表示までの秒数。初期値は0.125
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;;;;; GUI/Font

;; UI parts
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(if window-system
    (progn
      ;; UI parts
      (toggle-scroll-bar 0)
      (tool-bar-mode 0)
      (menu-bar-mode 0)))

;;;;; OS

;; exec-path-from-shell
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-variables '("PATH" "GOPATH"))
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; dired
(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))

;; MacのEmacsでファイル名を正しく扱うための設定
(use-package ucs-normalize
  :config
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)
  ;; Macだけに読み込ませる内容を書く
  :if (eq system-type 'darwin))

;;;;; Icons

(use-package all-the-icons
  :defer t)

;;;;; posframe

(use-package posframe
  :ensure t)

;;;; Key-bindings

;; C-mにnewline-and-indentを割り当てる
(global-set-key (kbd "C-m") 'newline-and-indent)

;; "C-t"でウィンドウを切り替える
(global-set-key (kbd "C-t") 'other-window)

;; text scaling
(define-globalized-minor-mode
  global-text-scale-mode
  text-scale-mode
  (lambda () (text-scale-mode 1)))

(defun global-text-scale-adjust (inc) (interactive)
       (text-scale-set 1)
       (kill-local-variable 'text-scale-mode-amount)
       (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
       (global-text-scale-mode 1)
       )

(global-set-key (kbd "M-0")
                '(lambda () (interactive)
                   (global-text-scale-adjust (- text-scale-mode-amount))
                   (global-text-scale-mode -1)))
(global-set-key (kbd "M-+")
                '(lambda () (interactive) (global-text-scale-adjust 1)))
(global-set-key (kbd "M--")
                '(lambda () (interactive) (global-text-scale-adjust -1)))


;;;; Search/Replace

;;;;; projectile
(use-package projectile
  :bind-keymap
  ;; projectileのプレフィックスキーをs-pに変更
  ("s-p" . projectile-command-map)
  :config
  ;;自動的にプロジェクト管理を開始
  (projectile-mode)
  ;; プロジェクト管理から除外するディレクトリを追加
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "elm-stuff")
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  :custom
  ;; プロジェクト情報をキャッシュする
  (projectile-enable-caching t)
  :ensure t)

;;;;; Helm
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files))
  :config
  (require 'helm-config)
  :ensure t)

(use-package helm-descbinds
  :after helm
  :config
  (helm-descbinds-mode)
  :ensure t)

(use-package helm-projectile
  :after projectile
  :config
  (helm-projectile-on)
  :custom
  (projectile-completion-system 'helm)
  :ensure t)

;;;;; tempbuf
;; https://www.emacswiki.org/emacs/tempbuf.el
(use-package tempbuf
  :diminish tempbuf-mode
  :disabled
  :ensure nil
  :load-path "site-lisp/tempbuf"
  :init
  (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'find-file-hooks 'turn-on-tempbuf-mode))

;;;; Check

;;;;; Flycheck
(use-package flycheck
  :config
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :ensure t)

;;;;; Flymake
(use-package flymake-posframe
  :after posframe
  :custom-face
  (flymake-posframe-foreground-face ((t (:foreground "white"))))
  :hook (flymake-mode . flymake-posframe-mode)
  :load-path "site-lisp/flymake-posframe")

;;;; Completion

;;;;; Yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

;;;;; Company
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
  (company-auto-complete 'ignore)
  (company-dabbrev-code-ignore-case t)
  ;; (company-dabbrev-code-modes
  ;;  '(prog-mode css-mode typescript-mode))
  (company-dabbrev-downcase nil)
  (company-emoji-insert-unicode nil)
  (company-flx-mode t)
  (company-global-modes t)
  (company-idle-delay 0.5)
  ;; (company-echo-delay 0)
  (company-lsp-cache-candidates 'auto)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-tooltip-idle-delay 0.1)
  (company-tooltip-limit 10)
  ;; (company-tooltip-minimum 10)
  :defer nil
  :ensure t)

(use-package company-box
  :custom
  (company-box-backends-colors nil)
  (company-box-doc-delay 0.3)
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  (company-box-show-single-candidate t)
  (company-box-max-candidates 20)
  :disabled
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package company-quickhelp
  :after company
  :custom
  (company-quickhelp-delay 0.8)
  :defines company-quickhelp-delay
  :disabled
  :ensure t
  :hook
  (global-company-mode . company-quickhelp-mode))

;;;; Git

;;;;; diffview
(use-package diffview
  :bind ("M-g v" . my/diffview-dwim)
  :commands (diffview-region diffview-current)
  :disabled
  :ensure t
  :preface
  (defun my/diffview-dwim ()
    (interactive)
    (if (region-active-p)
        (diffview-region)
      (diffview-current))))

;;;;; magit
(use-package magit
  :bind
  ("C-x g" . magit-status)
  :custom
  (magit-auto-revert-mode nil)
  :ensure t)

;;;;; diff-hl
(use-package diff-hl
  :config
  (global-diff-hl-mode t)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :ensure t)

;;;; Language

;;;;; add-node-modules-path
(use-package add-node-modules-path
  :ensure t
  :hook (elm-mode typescript-mode web-mode))

;;;;; Language Server Protocol
(use-package lsp-mode
  :commands lsp
  :config
   (require 'lsp-clients)
  :custom
  ;; debug
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)
  ;; general
  (lsp-auto-guess-root t)
  (lsp-document-sync-method nil) ; none, full, incremental, or nil
  (lsp-enable-snippet t)
  (lsp-prefer-flymake nil)
  (lsp-response-timeout 10)
  ;; elm
  (lsp-elm-elm-path (my/file-exists-fallback "./node_modules/.bin/elm" "elm"))
  (lsp-elm-elm-format-path (my/file-exists-fallback "./node_modules/.bin/elm-format" "elm-format"))
  (lsp-elm-elm-test-path (my/file-exists-fallback "./node_modules/.bin/elm-test" "elm-test"))
  (lsp-elm-elm-analyse-trigger "never")
  :ensure t
  :hook ((web-mode scss-mode typescript-mode go-mode python-mode elm-mode) . lsp)
  :preface
  (defun my/file-exists-fallback (path fallback)
    "If not, fall back"
    (if (file-exists-p path)
        path
      fallback)))

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
  (lsp-ui-doc-position 'at-point) ; top, bottom, or at-point
  (lsp-ui-doc-max-width 120)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit t)
  ;; lsp-ui-flycheck
  (lsp-ui-flycheck-enable t)
  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-code-actions t)
  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable nil)
  (lsp-ui-imenu-kind-position 'top)
  ;; lsp-ui-peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'on-demand) ; never, on-demand, or always
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
  (company-lsp-cache-candidates t) ; auto, t(always using a cache), or nil
  (company-lsp-enable-snippet t)
  (company-lsp-enable-recompletion nil)
  :ensure t)

;;;;; eldoc
(use-package eldoc
  :config
  (defun my/emacs-lisp-mode-hook ()
  "Hooks for Emacs Lisp mode."
  (setq eldoc-idle-delay 0.2)
  (setq eldoc-echo-area-use-multiline-p t)
  (turn-on-eldoc-mode))
  (add-hook 'emacs-lisp-mode #'my/emacs-lisp-mode-hook))

;;;;; Web
(use-package web-mode
  :config
  ;; 自動的にweb-modeを起動したい拡張子を追加する
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist
               '("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))
  ;; (add-to-list 'web-mode-comment-formats '("javascript" . "//"))
  ;; (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  :custom
  (web-mode-enable-auto-pairing nil)
  (web-mode-markup-indent-offset 2) ; HTMLのインデイント
  (web-mode-css-indent-offset 2) ; CSSのインデント
  (web-mode-code-indent-offset 2) ; JS, PHP, Rubyなどのインデント
  (web-mode-comment-style 2) ; web-mode内のコメントのインデント
  (web-mode-style-padding 1) ; <style>内のインデント開始レベル
  (web-mode-script-padding 1) ; <script>内のインデント開始レベル
  :ensure t)

;;;;; JavaScript
(use-package js2-mode
  :custom (js-indent-level 2)
  :ensure t
  :interpreter "node"
  :mode ("\\.js\\'" "\\.pac\\'"))

;;;;; SCSS
(use-package scss-mode
  :custom
  (scss-compile-at-save nil)
  :ensure t
  :mode "\\.scss$")

;;;;; JSON
(use-package json-mode
  :custom (js-indent-level 2)
  :ensure t
  :mode ("\\.json\\'"))

;;;;; TypeScript
(use-package typescript-mode
  :config
  (defun my/typescript-mode-hook ()
    "Hooks for typescript mode."
    (setq typescript-indent-level 2))

  (add-hook 'typescript-mode-hook 'my/typescript-mode-hook)
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode))

(use-package tide
  :after (typescript-mode company flycheck)
  :ensure t
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :mode ("\\.ts\\'" . typescript-mode))

;;;;; Elm
(use-package elm-mode
  :custom
  (elm-format-on-save nil)
  :ensure t
  :mode ("\\.elm\\'" . elm-mode))

;;;;; Go
(use-package go-mode
  :commands (go-set-project godoc gofmt gofmt-before-save)
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook #'go-set-project)
  :ensure t
  :mode "\\.go\\'")

;;;;; Python
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

;;;;; YAML
(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

;;;; UI

;;;;; doom-themes
(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :config
  (load-theme 'doom-spacegrey t)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  :ensure t)

;;;;; doom-modeline
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

;;;;; dimmer
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

;;;;; beacon
(use-package beacon
  :config
  (setq beacon-color "green")
  (beacon-mode t)
  :ensure t)

(provide 'init)
;;; init.el ends here
