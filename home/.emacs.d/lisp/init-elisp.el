;;; init-elisp.el -- Emascs Lisp setup

;;; Commentary:
;;; Emacs Lisp editing setup for Emacs
(require-package 'paredit)

(require 'paredit)

;;; Code:
(add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "Elisp")))
(add-hook 'lisp-mode-hook 'paredit-mode)

(diminish 'paredit-mode)

(provide 'init-elisp)
;;; init-elisp.el ends here
