;;; init-lisp.el -- Lisp and like setup

;;; Commentary:
;;; Lisp editing setup for Emacs
;;; Code:
(use-package aggressive-indent
  :commands (aggressive-indent-mode)
  :init
  (add-hook 'cider-repl-mode-hook 'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'lisp-mode-hook 'aggressive-indent-mode))

(use-package paredit
  :commands (paredit-mode)
  :init
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  :diminish paredit-mode)

(add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "Elisp")))

(provide 'init-lisp)
;;; init-lisp.el ends here
;; fuzzy find in current project with ,t
