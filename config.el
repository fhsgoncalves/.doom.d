;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Fernando Goncalves"
      user-mail-address "fernando.hsgoncalves@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "Monaco" :size 14 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Monaco" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;;(setq doom-theme 'doom-one)
;;(setq doom-theme 'doom-material)
;;(setq doom-theme 'doom-nord)
;;(set doom-theme 'doom-tomorrow-night)
(set doom-theme 'doom-palenight)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

; examples of key bindings:
; (map! :leader "rb" #'+eval/buffer) 
; (map! "M-r" #'+eval/buffer)

(map! "M-." #'lsp-find-definition)
(map! "M-," #'lsp-find-references)

(map! "M-;" #'comment-or-uncomment-region-or-line)

(map! :leader "wl" #'+workspace/load)

;;  ---------------------------
;;  term
;;  ---------------------------
(setq term-prompt-regexp "^[^#$%>\\n]*[#$%>] *")

;;  ---------------------------
;;  autosave
;;  ---------------------------
(setq auto-save-default nil)


;;  ---------------------------
;;  lsp-ui
;;  ---------------------------
(setq lsp-ui-sideline-enable nil)

;;  ---------------------------
;;  lsp
;;  ---------------------------
(setq company-minimum-prefix-length 3)

;;  ---------------------------
;;  lsp eslint
;;  ---------------------------
(setq lsp-eslint-auto-fix-on-save nil)
(setq lsp-eslint-quiet t)
;(setq lsp-eslint-run "onSave")


;;  ---------------------------
;;  backward-forward
;;  ---------------------------
(backward-forward-mode t)

;;  ---------------------------
;;  typescript
;;  ---------------------------
(setq js-indent-level 2)
(setq typescript-indent-level 2)

;;  ---------------------------
;;  hooks
;;  ---------------------------
(add-hook 'before-save-hook 'lsp-format-buffer)
;;(add-hook 'before-save-hook 'lsp-eslint-apply-all-fixes)
;;(add-hook 'before-save-hook 'lsp-format-buffer)

;(add-hook 'before-save-hook 'tide-format-before-save)
;(setq lsp-eslint-auto-fix-on-save t)


;;  ---------------------------
;;  key modifiers
;;  ---------------------------
(setq mac-command-modifier      'none
      ns-command-modifier       'none
      mac-option-modifier       'meta
      ns-option-modifier        'meta
      mac-right-option-modifier 'meta ;right option = left Window key on my external keyboard
      ns-right-option-modifier  'meta)

;;  ---------------------------
;;  lsp - elixir
;;  ---------------------------
(use-package lsp-mode
  :commands lsp
  :ensure t
  :diminish lsp-mode
  :hook
  (elixir-mode . lsp)
  :init
  (add-to-list 'exec-path "/Users/fernando.henrique/programs/elixir-ls"))

;;  ---------------------------
;;  vterm
;;  ---------------------------
(setq vterm-always-compile-module t)
(setq vterm-max-scrollback 10000)
(setq vterm-kill-buffer-on-exit t)
