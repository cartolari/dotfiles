;;; init-lisp.el -- Lisp and like setup

;;; Commentary:
;;; Lisp editing setup for Emacs
;;; Code:
(defvar my-lisp-modes
  '(cider-repl-mode clojure-mode emacs-lisp-mode lisp-mode))
(use-package aggressive-indent
  :commands (aggressive-indent-mode)
  :init
  (dolist (lisp my-lisp-modes)
    (add-hook (make-symbol (concat (symbol-name lisp) "-hook")) 'aggressive-indent-mode))
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(use-package paredit
  :commands (paredit-mode)
  :init
  (dolist (lisp my-lisp-modes)
    (add-hook (make-symbol (concat (symbol-name lisp) "-hook")) 'paredit-mode))
  :diminish paredit-mode)

(add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "Elisp")))

(provide 'init-lisp)
;;; init-lisp.el ends here
