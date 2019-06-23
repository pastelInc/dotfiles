(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-box-backends-colors nil t)
 '(company-box-max-candidates 20)
 '(company-box-show-single-candidate t)
 '(company-echo-delay 0 t)
 '(company-idle-delay 0)
 '(company-lsp-async t)
 '(company-lsp-cache-candidates t)
 '(company-lsp-enable-recompletion t)
 '(company-lsp-enable-snippet t)
 '(company-minimum-prefix-length 2)
 '(company-quickhelp-delay 0.8)
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "c74fff70a4cc37e2348dd083ae572c8b4baab4f6fb86adae5e0f2139a63e9a96" "2beaaef4f47f22c89948fdb3859799f8f2b64c1282ec21d71d6df49d68e68862" default)))
 '(dimmer-exclusion-regexp-list
   (quote
    (".*Minibuf.*" ".*which-key.*" ".*NeoTree.*" ".*Messages.*" ".*Async.*" ".*Warnings.*" ".*LV.*" ".*Ilist.*")) t)
 '(dimmer-fraction 0.5)
 '(doom-modeline-buffer-file-name-style (quote truncate-with-project) t)
 '(doom-modeline-icon t t)
 '(doom-modeline-major-mode-icon nil t)
 '(doom-modeline-minor-modes nil t)
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(frame-background-mode (quote dark))
 '(js-indent-level 2 t)
 '(lsp-auto-guess-root t)
 '(lsp-document-sync-method nil)
 '(lsp-elm-server-install-dir "~/.elm-language-server")
 '(lsp-enable-snippet t)
 '(lsp-prefer-flymake t)
 '(lsp-print-io t)
 '(lsp-print-performance nil)
 '(lsp-response-timeout 10)
 '(lsp-trace t t)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-include-signature nil)
 '(lsp-ui-doc-max-height 30)
 '(lsp-ui-doc-max-width 120)
 '(lsp-ui-doc-position (quote at-point))
 '(lsp-ui-doc-use-childframe t)
 '(lsp-ui-doc-use-webkit t)
 '(lsp-ui-flycheck-enable nil t)
 '(lsp-ui-imenu-enable t)
 '(lsp-ui-imenu-kind-position (quote top))
 '(lsp-ui-peek-enable t)
 '(lsp-ui-peek-fontify (quote on-demand))
 '(lsp-ui-peek-list-width 50)
 '(lsp-ui-peek-peek-height 20)
 '(lsp-ui-sideline-enable nil)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-show-code-actions t)
 '(lsp-ui-sideline-show-diagnostics nil)
 '(lsp-ui-sideline-show-hover t)
 '(lsp-ui-sideline-show-symbol t)
 '(magit-auto-revert-mode nil t)
 '(package-selected-packages
   (quote
    (yasnippet magit diffview json-mode doom-modeline posframe beacon lsp-ui use-package python-mode company-box sublime-theme dimmer dtrt-indent scss-mode ## company-lsp lsp-mode yaml-mode go-mode add-node-modules-path exec-path-from-shell nova-theme elm-mode web-mode helm-projectile projectile auto-complete typescript-mode js2-mode sass-mode company-quickhelp company helm-descbinds helm)))
 '(safe-local-variable-values (quote ((elm-format-elm-version . "0.18"))))
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t)
 '(vc-annotate-background "#3C4C55")
 '(vc-annotate-color-map
   (\`
    ((20 \, "#DF8C8C")
     (40 \, "#ffffffff0000")
     (60 \, "#ffffffff0000")
     (80 \, "#ffffffff0000")
     (100 \, "#ffffffff0000")
     (120 \, "#F2C38F")
     (140 \, "#ffffffff0000")
     (160 \, "#ffffffff0000")
     (180 \, "#ffffffff0000")
     (200 \, "#ffffffff0000")
     (220 \, "#DADA93")
     (240 \, "#ffffffff0000")
     (260 \, "#ffffffff0000")
     (280 \, "#ffffffff0000")
     (300 \, "#ffffffff0000")
     (320 \, "#A8CE93")
     (340 \, "#ccccffff3332")
     (360 \, "#9999ffff6666")
     (380 \, "#6666ffff9999")
     (400 \, "#3333ffffcccc")
     (420 \, "#83AFE5")
     (440 \, "#3332ccccffff")
     (460 \, "#66669999ffff")
     (480 \, "#99996666ffff")
     (500 \, "#cccc3333ffff")
     (520 \, "#9A93E1"))))
 '(vc-annotate-very-old-color "#cccc0000cccc")
 '(web-mode-code-indent-offset 2)
 '(web-mode-comment-style 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-script-padding 1)
 '(web-mode-style-padding 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(company-scrollbar-bg ((t (:background "#333"))))
 '(company-scrollbar-fg ((t (:background "deep sky blue"))))
 '(company-tooltip ((t (:inherit default :foreground "#c0c5ce" :background "#333"))))
 '(company-tooltip-annotation ((t (:foreground "white smoke"))))
 '(company-tooltip-annotation-selection ((t (:foreground "black"))))
 '(company-tooltip-common ((t (:foreground "orange"))))
 '(company-tooltip-common-selection ((t (:foreground "black"))))
 '(company-tooltip-selection ((t (:foreground "black" :background "deep sky blue"))))
 '(flymake-posframe-foreground-face ((t (:foreground "white"))))
 '(show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c")))))
