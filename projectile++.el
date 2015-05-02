(require 'projectile)

(defun projectile-cleanup-project-buffers ()
  (interactive)
  (dolist (buffer (projectile-project-buffer-names))
    (condition-case nil
        (with-current-buffer buffer
          (esk-cleanup-buffer))
        (buffer-read-only nil))))

(defun qt-current-project-file-full-paths ()
  (let ((root (projectile-project-root)))
    (mapcar (lambda (filename)
              (expand-file-name filename root))
            (projectile-current-project-files))))

(defun qt-add-dev-ns (ns)
  (setq cljr-auto-sort-ns nil)
  (ignore-errors
    (cljr-add-require-to-ns)
    (insert ns)
    (yas-exit-all-snippets)
    (cljr-sort-ns))
  (setq cljr-auto-sort-ns t))

(defun qt-untabify-buffer ()
  (untabify (point-min) (point-max)))

(defun qt-indent-buffer ()
  (indent-region (point-min) (point-max)))

(defun qt-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (qt-indent-buffer)
  (qt-untabify-buffer)
  (delete-trailing-whitespace))

(defun qt-cleanup-file (filename)
  (interactive "sFile: ")
  (find-file filename)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (progn
        (qt-cleanup-buffer)
        (save-buffer)))))

(defun qt-current-project-file-full-paths ()
  (let ((root (projectile-project-root)))
    (mapcar (lambda (filename)
              (expand-file-name filename root))
            (projectile-current-project-files))))

(defun qt-cleanup-project-files (project-name)
  (interactive "sproject name (i.e. 'itsman'): ")
  (dolist (filename (qt-current-project-file-full-paths))
    (when (and (s-ends-with? "clj" filename)
               (not (s-contains? "checkouts" filename)))
      (ignore-errors
        (find-file filename)
        (qt-add-dev-ns (concat "[" project-name ".dev :refer :all]"))
        (cljr-remove-unused-requires)
        (qt-cleanup-file filename)))))

(defun projectile-cleanup-project-files ()
  (interactive)
  (dolist (filename (qt-current-project-file-full-paths))
    (cleanup-file filename)))

(defun projectile-cleanup-project-clj-files ()
  (interactive)
  (dolist (filename (qt-current-project-file-full-paths))
    (when (s-ends-with? "clj" filename)
      (ignore-errors
        (find-file filename)
        (cljr-remove-unused-requires)
        (cljr-sort-ns)
        (cleanup-file filename))))) 

(global-set-key (kbd "C-c N") 'projectile-cleanup-project-buffers)
(global-set-key (kbd "C-x M-f") 'projectile-find-file)

;;(setq projectile-indexing-method 'native)
(setq projectile-globally-ignored-directories 
      (append projectile-globally-ignored-directories '(".git" ".m2" "target")))


