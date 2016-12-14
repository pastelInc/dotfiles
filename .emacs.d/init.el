;;; init.el --- My Emacs Initialization/Customization file  -*- lexical-binding: t -*-

;; Filename: init.el
;; Description: My Emacs Initialization/Customization file
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;;; Change Log:

;;; Code:

;; @ load-path

;; load-pathの追加関数
;; (defun add-to-load-path (&rest paths)
;;   (let (path)
;;     (dolist (path paths paths)
;;       (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
;;         (add-to-list 'load-path default-directory)
;;         (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
;;             (normal-top-level-add-subdirs-to-load-path))))))

;; elisp
;; (add-to-load-path "elisp")

;; @ general

;; 文字コード
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
                             :family "Ricty"
                             :height 140)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty")))))

;; スタートアップ非表示
(setq inhibit-startup-screen t)

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; ツールバー非表示
(if window-system (progn
                    (tool-bar-mode -1)))

;; メニューバーを非表示
(if window-system (progn
                    (menu-bar-mode -1)))

;; スクロールバー非表示
(if window-system (progn
                    (set-scroll-bar-mode nil)))

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; 行番号表示
(global-linum-mode t)

;; 行番号フォーマット
(defvar linum-format "%4d")

;; 括弧の範囲内を強調表示
(show-paren-mode t)
(defvar show-paren-delay 0)
(defvar show-paren-style 'expression)

;; 行末の空白を強調表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; タブ幅
(custom-set-variables '(tab-width 4))

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; 最近使ったファイルをメニューに表示
(recentf-mode t)

;; 最近使ったファイルの表示数
(defvar recentf-max-menu-items 10)

;; 最近開いたファイルの保存数を増やす
(defvar recentf-max-saved-items 3000)

;; ミニバッファの履歴を保存する
(savehist-mode 1)

;; ミニバッファの履歴の保存数を増やす
(setq history-length 3000)

;; バックアップを残さない
(setq make-backup-files nil)

;; 行間
(setq-default line-spacing 0)

;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
;; shell-mode
(defvar comint-scroll-show-maximum-output t)

;; フレームの透明度
(set-frame-parameter (selected-frame) 'alpha '(0.87))

;; モードラインに行番号表示
(line-number-mode t)

;; モードラインに列番号表示
(column-number-mode t)

;; Show paren
(show-paren-mode t)

;; White space
(setq-default show-trailing-whitespace t)

;; ffapでワイルドカードを指定するとdiredを開くようにする
(setq ffap-pass-wildcards-to-dired t)

;; C-x C-fなどをffap関係のコマンドに割り当てる
(ffap-bindings)

;; @ modeline

;; モードラインの割合表示を総行数表示
(defvar my-lines-page-mode t)
(defvar my-mode-line-format)

(when my-lines-page-mode
  (setq my-mode-line-format "%d")
  (if size-indication-mode
      (setq my-mode-line-format (concat my-mode-line-format " of %%I")))
  (cond ((and (eq line-number-mode t) (eq column-number-mode t))
         (setq my-mode-line-format (concat my-mode-line-format " (%%l,%%c)")))
        ((eq line-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format " L%%l")))
        ((eq column-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format " C%%c"))))

  (setq mode-line-position
        '(:eval (format my-mode-line-format
                        (count-lines (point-max) (point-min))))))

;; @ package manager
(package-initialize)

;; @ cask
(when (or (require 'cask "~/.cask/cask.el" t)
          (require 'cask nil t))
  (cask-initialize))

;; @ use-package.el
(require 'use-package)

;; @ key bind
;; use bind-key package
(progn
  (bind-key "<f8>" 'neotree-toggle)
  (bind-key* "C-c <left>"  'windmove-left)
  (bind-key* "C-c <down>"  'windmove-down)
  (bind-key* "C-c <up>"    'windmove-up)
  (bind-key* "C-c <right>" 'windmove-right))

;; @ anything.el
;; (use-package  anything
;;   :config
;;   (define-key global-map (kbd "\C-x b") 'anything))

;; (use-package anything-startup)

;; @ auto-complete.el
(use-package auto-complete
  :defer t
  :config
  ;; 自動的に有効にする
  (global-auto-complete-mode t)
  (ac-set-trigger-key "TAB")
  ;; 補完メニュー表示時にC-n/C-pで補完候補選択
  (setq ac-use-menu-map t)
  ;; 曖昧マッチ
  (setq ac-use-fuzzy t))
(use-package auto-complete-config
  :defer t
  :config
  (ac-config-default))

;; @ color theme
(use-package moe-theme
  :config
  (load-theme 'moe-dark t))

;; @ company
(use-package company
  :defer t
  :config
  ;; C-n, C-pで補完候補を次/前の候補を選択
  (bind-key "C-n" 'company-select-next company-active-map)
  (bind-key "C-p" 'company-select-previous company-active-map)
  (bind-key "C-n" 'company-select-next company-search-map)
  (bind-key "C-p" 'company-select-previous company-search-map)
  ;; 1文字入力で補完されるように
  (setq company-minimum-prefix-length 1)
  ;; 候補の一番上でselect-previousしたら一番下に、一番下でselect-nextしたら一番上に行くように
  (setq company-selection-wrap-around t)
  ;; 色の設定
  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
                      :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray40"))

;; @ elscreen.el
(use-package elscreen
  :config
  ;; プレフィクスキーはC-t
  (defvar elscreen-prefix-key (kbd "C-t"))
  ;; タブの先頭に[X]を表示しない
  (defvar elscreen-tab-display-kill-screen nil)
  ;; header-lineの先頭に[<->]を表示しない
  (defvar elscreen-tab-display-control nil)
  (elscreen-start))

;; @ flycheck.el
(use-package flycheck
    :ensure t
    :init (global-flycheck-mode))

;; @ git-gutter.el
(use-package git-gutter :defer t)

;; @ helm.el
(use-package helm-config
  :defer t
  :init
  (bind-key "C-x b" 'helm-mini))

;; @ maxframe.el
;; 起動時にウィンドウ最大化
(use-package maxframe
  :config
  (add-hook 'window-setup-hook 'maximize-frame t))

;; @ neotree.el
(use-package neotree :defer t)

;; @ popwin.el
(use-package popwin
  :defer t
  :config
  (popwin-mode 1))

;; @ powerline.ela
(use-package powerline
  :config
  (set-face-attribute 'mode-line nil
                      :foreground "#fff"
                      :background "#FF0066"
                      :box nil)
  (set-face-attribute 'powerline-active1 nil
                      :foreground "#fff"
                      :background "#FF6699"
                      :inherit 'mode-line)
  (set-face-attribute 'powerline-active2 nil
                      :foreground "#000"
                      :background "#ffaeb9"
                      :inherit 'mode-line)
  (powerline-default-theme))

;; @ recentf-ext.el
(use-package recentf-ext
  :defer t
  :init
  ;; 起動画面で recentf を開く
  (add-hook 'after-init-hook (lambda()
                               (recentf-open-files)
                               ))
  (bind-key "C-x C-r" 'recentf-open-files)
  :config
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 10)
  (defvar recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1))


;; @ redo+.el
(use-package redo+
  :defer t
  :init
  (bind-key "C-_" 'redo))

;; @ zlc.el
(use-package zlc
  :defer t
  :config
  (zlc-mode t)
  (let ((map minibuffer-local-map))
    (bind-key "<down>"  'zlc-select-next-vertical map)
    (bind-key "<up>" 'zlc-select-previous-vertical map)
    (bind-key "<right>" 'zlc-select-next map)
    (bind-key "<left>" 'zlc-select-previous map)
    (bind-key "C-c" 'zlc-reset map)))

;; @ golang
;; (add-to-list 'exec-path (expand-file-name "/usr/local/bin"))
(add-to-list 'exec-path (expand-file-name "~/go/bin"))

(use-package go-mode
  :defer t
  :config
  (setq gofmt-command "goimports")
  (bind-keys :map go-mode-map
         ("M-." . godef-jump)
         ("M-," . pop-tag-mark))
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook '(lambda ()
                             (setq tab-width 2)
                             ;; GOROOT, GOPATH環境変数の読み込み
                             (let ((envs '("GOROOT" "GOPATH")))
                               (exec-path-from-shell-copy-envs envs))
                             (setq-default)
                             ;; indentの設定
                             (setq tab-width 2)
                             (setq standard-indent 2)
                             (setq indent-tabs-mode nil)))
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-autocomplete :ensure t)

(use-package go-eldoc
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-autocomplete
  :defer t
  :if (locate-library "go-mode"))

;; @ PHP
(use-package php-mode :defer t)

(defun my/setup-tide-mode ()
  "Set up tide mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; @ JavaScript
(use-package js2-mode
  :defer t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter ("node" . js2-mode)
  :config
  (progn
    (add-hook 'js2-mode-hook #'my/setup-tide-mode)
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
    (add-hook 'js2-mode-hook (lambda ()
                               (bind-key "M-j" 'join-line-or-lines-in-region js2-mode-map)))))

;; @ TypeScript
(use-package typescript
  :defer t
  :mode ("\\.ts\\'" . typescript-mode)
  :config
  (defvar typescript-indent-level 2)
  (progn
    (add-hook 'typescript-mode-hook 'company-mode)
    (add-hook 'typescript-mode-hook #'my/setup-tide-mode)))

(use-package ng2-mode
  :defer t
  :if (locate-library "typescript"))

(use-package tide
  :defer t
  :if (locate-library "typescript")
  :config
  ;; aligns annotation to the right hand side
  (defvar company-tooltip-align-annotations t)
  (progn
    ;; formats the buffer before saving
    (add-hook 'before-save-hook 'tide-format-before-save)))

;;; init.el ends here
