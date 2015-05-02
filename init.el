(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(fset 'gui-diff-last-failure
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([18 97 99 116 117 97 108 58 13 134217734 19 40 61 13 right 201326624 201326624 134217847 134217790 40 103 117 105 45 100 105 102 102 32 25 41] 0 "%d")) arg)))


(when (not package-archive-contents)
  (package-refresh-contents))


;;(load "~/.emacs.d/starter-.el")
(load "~/.emacs.d/starter-kit-misc.el")
(load "~/.emacs.d/starter-kit-defuns.el")
(load "~/.emacs.d/starter-kit.el")
(require 'starter-kit)

(require 'protobuf-mode)
(load "~/.emacs.d/floobits/floobits.el")

(require 'wgrep)

(load "~/.emacs.d/user.el")




(require 'projectile)
(require 'clj-refactor)
(defun projectile-cleanup-project-buffers ()
  (interactive)
  (dolist (buffer (projectile-project-buffer-names))
    (condition-case nil
        (with-current-buffer buffer
          (esk-cleanup-buffer))
        (buffer-read-only nil))))

(defun projectile-current-project-file-full-paths ()
  (let ((root (projectile-project-root)))
    (mapcar (lambda (filename)
              (expand-file-name filename root))
            (projectile-current-project-files))))

(defun projectile-cleanup-project-files ()
  (interactive)
  (dolist (filename (projectile-current-project-file-full-paths))
    (cleanup-file filename)))

(defun cljr-add-dev-ns (ns)
  (setq cljr-auto-sort-ns nil)
  (ignore-errors
    (cljr-add-require-to-ns)
    (insert ns)
    (yas-exit-all-snippets)
    (cljr-sort-ns))
  (setq cljr-auto-sort-ns t))

(defun cleanup-work-project (repo-namespace-prefix)
  (interactive "sEnter repo namespace prefix: ")
  (dolist (filename (projectile-current-project-file-full-paths))
    (when (and (s-ends-with? "clj" filename)
               (or
                (equal repo-namespace-prefix "french-castle")
                (equal repo-namespace-prefix "webstack")
                (and
                 (not (s-contains? "french-castle" filename))
                 (not (s-contains? "webstack" filename)))))
      (ignore-errors
        (find-file filename)
        (cljr-add-dev-ns (concat "[" repo-namespace-prefix ".dev :refer :all]"))
        (cljr-remove-unused-requires)
        (cleanup-file filename)))))

(defun projectile-cleanup-project-clj-files ()
  (interactive)
  (dolist (filename (projectile-current-project-file-full-paths))
    (when (s-ends-with? "clj" filename)
      (ignore-errors
        (find-file filename)
        (cljr-remove-unused-requires)
        (cljr-sort-ns)
        (cleanup-file filename)))))

(winner-mode)
(require 'multiple-cursors)
(multiple-cursors-mode)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-!") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(define-key clojure-mode-map (kbd "C->") 'cljr-thread)
(define-key clojure-mode-map (kbd "C-<") 'cljr-unwind)
(define-key clojure-mode-map (kbd "M-C->") 'cljr-thread-first-all)
(define-key clojure-mode-map (kbd "M-C-?") 'cljr-thread-last-all)


;; SCROLL 
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;;(setq mouse-wheel-progressive-speed t) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time



(load "~/.emacs.d/core.el")
(load "~/.emacs.d/cider-mode++.el")
(load "~/.emacs.d/clojure-mode++.el")
(load "~/.emacs.d/ido++.el")
(load "~/.emacs.d/magit++.el")
(load "~/.emacs.d/python++.el")

(load "~/.emacs.d/projectile++.el") 
(load "~/.emacs.d/org-mode++.el")
(load "~/.emacs.d/git-timemachine.el")
(load "~/.emacs.d/scss-mode.el")
(load "~/.emacs.d/web-mode.el")
(load "~/.emacs.d/catchall.el")

(load "~/.emacs.d/js++.el")

;;(load "~/.emacs.d/erc++.el")
;;(load "~/.emacs.d/ruby++.el")
;;(load "~/.emacs.d/floobits/floobits.el")
;;(load "~/.emacs.d/el/neotree/neotree.el")
;;(load "~/.emacs.d/jsx-mode.el")

;;(load "~/.emacs.d/user.el")
