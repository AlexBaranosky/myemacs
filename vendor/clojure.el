;; clojure
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))

;; cider
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-repl-pop-to-buffer-on-connect t)
(setq cider-popup-stacktraces t)
(setq cider-repl-popup-stacktraces t)
(setq cider-auto-select-error-buffer t)
(setq cider-repl-history-file "~/.emacs.d/cider-history")
(setq cider-repl-wrap-history t)


(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(require 'whitespace)
(add-hook 'clojure-mode-hook (lambda () (whitespace-mode t)))
(add-hook 'clojurescript-mode-hook (lambda () (whitespace-mode t)))

(setq whitespace-style '(face lines-tail trailing))
(setq whitespace-line-column 80)
