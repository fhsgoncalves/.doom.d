(put 'projectile-ag 'disabled nil)

(backward-forward-mode t)

(setq js-indent-level 2)
(setq typescript-indent-level 2)

;; format on save
(add-hook 'before-save-hook 'lsp-format-buffer)
(add-hook 'before-save-hook 'tide-format-before-save)

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

;;  ---------------------------
;;  typescript
;;  ---------------------------
(defun lsp-js-ts-rename-file ()
  "Rename current file and all it's references in other files."
  (interactive)
  (let* ((name (buffer-name))
         (old (buffer-file-name))
         (basename (file-name-nondirectory old)))
    (unless (and old (file-exists-p old))
      (error "Buffer '%s' is not visiting a file." name))
    (let ((new (read-file-name "New name: " (file-name-directory old) basename nil basename)))
      (when (get-file-buffer new)
        (error "A buffer named '%s' already exists." new))
      (when (file-exists-p new)
        (error "A file named '%s' already exists." new))
      (lsp--send-execute-command
       "_typescript.applyRenameFile"
       (vector (list :sourceUri (lsp--buffer-uri)
                     :targetUri (lsp--path-to-uri new))))
      (mkdir (file-name-directory new) t)
      (rename-file old new)
      (rename-buffer new)
      (set-visited-file-name new)
      (set-buffer-modified-p nil)
      (lsp-disconnect)
      (setq-local lsp-buffer-uri nil)
      (lsp)
      (lsp--info "Renamed '%s' to '%s'." name (file-name-nondirectory new)))))

;;  ---------------------------
;;  comments
;;  ---------------------------
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
