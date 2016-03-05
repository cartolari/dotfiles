;;; init-lisp.el -- Lisp and like setup

;;; Commentary:
;;; Lisp editing setup for Emacs
;;; Code:
(require 'use-package)

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
  (add-hook 'eval-expression-minibuffer-setup-hook 'paredit-mode)
  :diminish paredit-mode)

(use-package elisp-mode
  :ensure nil
  :config
  (defun eval-and-replace ()
    "Replace the preceding sexp with its value."
    (interactive)
    (backward-kill-sexp)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0)))))
  (add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "Elisp")))
  (define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-and-replace))

(provide 'init-lisp)
;;; init-lisp.el ends here
;; fuzzy find in current project with ,t
