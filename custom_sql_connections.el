;;; .dbconf parser

(defun ini-decode (ini_text) 
  ;; text -> alist
  (interactive)
  (if (not (stringp ini_text))
      (error "Must be a string"))
  (let ((lines (split-string ini_text "\n"))
    (section)
    (section-list)
    (alist))
    (dolist (l lines)
      ;; skip comments
      (unless (or (string-match "^;" l)
          (string-match "^[ \t]$" l))
    ;; catch sections
    (if (string-match "^\\[\\(.*\\)\\]$" l)
        (progn 
          (if section
          ;; add as sub-list
          (setq alist (cons `(,section . ,section-list) alist))
        (setq alist section-list))
          (setq section (match-string 1 l))
          (setq section-list nil)))
          ;; catch properties
          (if (string-match "^\\([^\s\t]+\\)[\s\t]*=[\s\t]*\\(.+\\)$" l)
          (let ((property (match-string 1 l))
            (value (match-string 2 l)))
            (progn 
              (setq section-list (cons `(,property . ,value) section-list)))))))
    (if section
    ;; add as sub-list
    (setq alist (cons `(,section . ,section-list) alist))
      (setq alist section-list))
    alist))

(defun read-ini (file)
  "Returns ini file as alist."
  (with-temp-buffer
    (insert-file-contents file)
    (ini-decode (buffer-string))))

(defun filter-alist (wanted-members alist)
  "Returns a copy of given alist, with only fields from wanted-members."
  (let ((result nil)
        (add-if-member (lambda (elt)
                         (when (member (car elt) wanted-members)
                           (add-to-list 'result elt t)))))
    (mapc add-if-member alist)
    result))

(defun merge-alist (original override)
  "Returns a union of original and override alist. On key conflict, the latter wins."
  (let ((result (copy-alist override))
        (add (lambda (elt)
               (setq result (add-to-list
                             'result elt t
                             (lambda (left right) (equal (car left) (car right))))))))
    (mapc add original)
    result))

(defun parse-dbconf-file (file-path)
  "Returns list of hosts with clients' section applied to all hosts."
  (let ((hosts nil)
        (global nil)
        (fields '("user" "host" "database" "password" "port"))
        (section-parse (lambda(section)
                         (if (equal (car section) "client")
                             (setq global (filter-alist fields (cdr section)))
                           (let ((host (car section))
                                 (config (filter-alist fields (cdr section))))
                             (when config (add-to-list 'hosts (cons host config) t))))))
        (merge-host-with-global (lambda (host)
                                  (cons (car host) (merge-alist global (cdr host))))))
    (mapc section-parse (read-ini file-path))
    (mapcar merge-host-with-global hosts)))

(defun dbconf-to-sql-connection (config)
  (let ((parse-keys-and-values
         (lambda (config)
           (let ((head (car config))
                 (tail (cdr config)))
             (cons
              head
              (mapcar
               (lambda (element)
                (let ((key (car element))
                      (value (cdr element)))
                  (cond ((equal key "host") (list 'sql-server value))
                        ((equal key "port") (list 'sql-port (string-to-number value)))
                        ((equal key "user") (list 'sql-user value))
                        ((equal key "password") (list 'sql-password value))
                        ((equal key "database") (list 'sql-database value))
                        ((equal key "product") (list 'sql-product value))
                        (t (error (format "Unknown key %s" key))))))
               tail))))))
    (mapcar parse-keys-and-values config)))

;;; Actually populating sql-connection-alist
(setq sql-connection-alist
      (append
       (dbconf-to-sql-connection (parse-dbconf-file "~/.dbconf"))))

