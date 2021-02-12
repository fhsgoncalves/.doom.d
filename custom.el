(put 'projectile-ag 'disabled nil)

(backward-forward-mode t)

(setq js-indent-level 2)
(setq typescript-indent-level 2)

;; format on save
(add-hook 'before-save-hook 'lsp-format-buffer)

(setq mac-command-modifier      'none
      ns-command-modifier       'none
      mac-option-modifier       'meta
      ns-option-modifier        'meta
      mac-right-option-modifier 'meta ;right option = left Window key on my external keyboard
      ns-right-option-modifier  'meta)

;;  ---------------------------
;;  typescript
;;  ---------------------------

;; relative import
(eval-after-load "tide"
  (lambda ()
    (plist-put tide-user-preferences :importModuleSpecifierPreference "relative")
    (plist-put tide-user-preferences :includeCompletionsForModuleExports t)
    (plist-put tide-user-preferences :includeCompletionsWithInsertText t)
    (plist-put tide-user-preferences :allowTextChangesInNewFiles t)))


;;  ---------------------------
;;  sql-mode
;;  ---------------------------
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))
(load-file "~/.doom.d/custom_sql_connections.el")

;;  ---------------------------
;;  sqlformat
;;  ---------------------------
(setq sqlformat-command 'pgformatter)
(setq sqlformat-args '("-s2" "-g"))
(add-hook 'sql-mode-hook 'sqlformat-on-save-mode)


