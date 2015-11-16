
;; (defun magit-toggle-whitespace ()
;;   (interactive)
;;   (if (member "-w" magit-diff-options)
;;       (magit-dont-ignore-whitespace)
;;     (magit-ignore-whitespace)))

;; (defun magit-ignore-whitespace ()
;;   (interactive)
;;   (add-to-list 'magit-diff-options "-w")
;;   (magit-refresh))

;; (defun magit-dont-ignore-whitespace ()
;;   (interactive)
;;   (setq magit-diff-options (remove "-w" magit-diff-options))
;;   (magit-refresh))

;; (define-key magit-mode-map (kbd "W") 'magit-toggle-whitespace)
(setq magit-highlight-whitespace nil)

(global-set-key (kbd "C-c g") 'magit-status)
(setq git-commit-summary-max-length 1000)
(setq git-commit-fill-column 1000)
