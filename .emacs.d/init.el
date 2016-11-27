;;; init.el --- My Emacs Initialization/Customization file  -*- lexical-binding: t -*-

;; Filename: init.el
;; Description: My Emacs Initialization/Customization file
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; ------------------------------------------------------------------------
;; @ load-path

;; load-pathの追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; elisp
;; (add-to-load-path "elisp")

;; ------------------------------------------------------------------------
;; @ general

;; common lisp
;; (require 'cl)

;; 文字コード
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

;; スタートアップ非表示
(setq inhibit-startup-screen t)

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; ツールバー非表示
(if window-system (progn
                    (tool-bar-mode -1)))

;; メニューバーを非表示
(if window-system (progn
                    (menu-bar-mode -1)))

;; スクロールバー非表示
(if window-system (progn
                    (set-scroll-bar-mode nil)))

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; 行番号表示
(global-linum-mode t)
(set-face-attribute 'linum nil
                    :foreground "#800"
                    :height 0.9)

;; 行番号フォーマット
(setq linum-format "%4d")

;; 括弧の範囲内を強調表示
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

;; 括弧の範囲色
(set-face-background 'show-paren-match-face "#500")

;; 選択領域の色
(set-face-background 'region "#555")

;; 行末の空白を強調表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; タブ幅
(custom-set-variables '(tab-width 4))

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; 最近使ったファイルをメニューに表示
(recentf-mode t)

;; 最近使ったファイルの表示数
(setq recentf-max-menu-items 10)

;; 最近開いたファイルの保存数を増やす
(setq recentf-max-saved-items 3000)

;; ミニバッファの履歴を保存する
(savehist-mode 1)

;; ミニバッファの履歴の保存数を増やす
(setq history-length 3000)

;; バックアップを残さない
(setq make-backup-files nil)

;; 行間
(setq-default line-spacing 0)

;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell-mode

;; フレームの透明度
(set-frame-parameter (selected-frame) 'alpha '(0.87))

;; モードラインに行番号表示
(line-number-mode t)

;; モードラインに列番号表示
(column-number-mode t)

;; C-Ret で矩形選択
;; 詳しいキーバインド操作：http://dev.ariel-networks.com/articles/emacs/part5/
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; ------------------------------------------------------------------------
;; @ modeline

;; モードラインの割合表示を総行数表示
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

;; ------------------------------------------------------------------------
;; @ initial frame maximize

;; 起動時にウィンドウ最大化
;; http://www.emacswiki.org/emacs/FullScreen#toc12
;; (defun jbr-init ()
;;   "Called from term-setup-hook after the default
;;    terminal setup is
;;    done or directly from startup if term-setup-hook not
;;    used.  The value
;;    0xF030 is the command for maximizing a window."
;;   (interactive)
;;   (w32-send-sys-command #xf030)
;;   (ecb-redraw-layout)
;;   (calendar))

;; (let ((ws window-system))
;;   (cond ((eq ws 'w32)
;;          (set-frame-position (selected-frame) 0 0)
;;          (setq term-setup-hook 'jbr-init)
;;          (setq window-setup-hook 'jbr-init))
;;         ((eq ws 'ns)
;;          ;; for MacBook Air(Late2010) 11inch display
;;          (set-frame-position (selected-frame) 0 0)
;;          (set-frame-size (selected-frame) 95 47))))

;; ------------------------------------------------------------------------
;; @ key bind

;; バックスラッシュ
(define-key global-map (kbd "M-|") "\\")

;; globalなC-zを無効化
(global-unset-key "\C-z")

;; ------------------------------------------------------------------------
;; @ color theme

(load-theme 'deeper-blue t)

;; ------------------------------------------------------------------------
;; @ package manager

(require 'package)
(package-initialize)

;; ------------------------------------------------------------------------
;; @ cask

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; ------------------------------------------------------------------------
;; @ use-package.el

(require 'use-package)

;; ------------------------------------------------------------------------
;; @ anything.el

(use-package  anything
  :config
  (define-key global-map (kbd "\C-x b") 'anything))

;; ------------------------------------------------------------------------
;; @ auto-complete.el

(use-package auto-complete
  :config
  (ac-config-default)
  ;; 自動的に有効にする
  (global-auto-complete-mode t)
  (ac-set-trigger-key "TAB")
  ;; 補完メニュー表示時にC-n/C-pで補完候補選択
  (setq ac-use-menu-map t)
  ;; 曖昧マッチ
  (setq ac-use-fuzzy t))

;; ------------------------------------------------------------------------
;; @ browse-kill-ring.el

(use-package browse-kill-ring
  :config
  (global-set-key "\C-cy" 'browse-kill-ring))

;; ------------------------------------------------------------------------
;; @ elscreen.el

(use-package elscreen
  :config
  ;; プレフィクスキーはC-t
  (setq elscreen-prefix-key (kbd "C-t"))
  (elscreen-start)
  ;; タブの先頭に[X]を表示しない
  (setq elscreen-tab-display-kill-screen nil)
  ;; header-lineの先頭に[<->]を表示しない
  (setq elscreen-tab-display-control nil))

;; ------------------------------------------------------------------------
;; @ flycheck.el

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; ------------------------------------------------------------------------
;; @ git-gutter.el

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

;; ------------------------------------------------------------------------
;; @ pbcopy.el

(use-package pbcopy
  :config
  (turn-on-pbcopy))

;; ------------------------------------------------------------------------
;; @ popwin.el

(use-package popwin
  :config
  (popwin-mode 1))

;; ------------------------------------------------------------------------
;; @ powerline.el

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


;; ------------------------------------------------------------------------
;; @ recentf-ext.el

;; 自動保存
(use-package recentf-ext
  :config
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1)
  ;; 起動画面で recentf を開く
  (add-hook 'after-init-hook (lambda()
                               (recentf-open-files)
                               ))
  ;; キーバインド
  (global-set-key (kbd "C-x C-r") 'recentf-open-files))


;; ------------------------------------------------------------------------
;; @ redo+.el

;; redoできるようにする
;; http://www.emacswiki.org/emacs/redo+.el
(use-package redo+
  :config
  (define-key global-map (kbd "C-_") 'redo))

;; ------------------------------------------------------------------------
;; @ zlc.el

(use-package zlc
  :config
  (zlc-mode t)
  (let ((map minibuffer-local-map))
    (define-key map (kbd "<down>")  'zlc-select-next-vertical)
    (define-key map (kbd "<up>")    'zlc-select-previous-vertical)
    (define-key map (kbd "<right>") 'zlc-select-next)
    (define-key map (kbd "<left>")  'zlc-select-previous)

    (define-key map (kbd "C-c") 'zlc-reset)
    ))

;; ------------------------------------------------------------------------
;;  @ golang

;;(add-to-list 'exec-path (expand-file-name "/usr/local/opt/go/libexec/bin"))
;;(add-to-list 'exec-path (expand-file-name "~/go/bin"))
(add-hook 'go-mode-hook
          (lambda ()
            ;; GOROOT, GOPATH環境変数の読み込み
            (let ((envs '("GOROOT" "GOPATH")))
              (exec-path-from-shell-copy-envs envs))
            (setq-default)
            ;; indentの設定
            (setq tab-width 2)
            (setq standard-indent 2)
            (setq indent-tabs-mode nil)
            ;; godef keybind
            (local-set-key (kbd "M-.") 'godef-jump)
            (local-set-key (kbd "M-,") 'pop-tag-mark)
            ))
(eval-after-load "go-mode"
  '(progn
     (require 'go-autocomplete)
     (add-hook 'go-mode-hook 'go-eldoc-setup)))

;; (provide 'init) ; make sure it only load once.

;; 幾つかのモードは、巨大ファイルでは動作が重くなるのでオフにする。

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; init.el ends here
