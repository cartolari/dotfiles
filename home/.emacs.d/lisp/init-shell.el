;;; init-shell.el -- Setup shell inside Emacs

;;; Commentary:
;;; Eshell and term setup

;;; Code:
(require 'use-package)

(use-package eshell
  :config
  (add-hook 'eshell-mode-hook (lambda () (company-mode -1)))
  (add-hook 'eshell-mode-hook
            (lambda ()
              (substitute-key-definition
               'eshell-list-history 'helm-eshell-history eshell-mode-map)))
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map
                [remap eshell-pcomplete] 'helm-esh-pcomplete)))
  :ensure nil)

(use-package shell
  :config
  (bind-keys :map shell-mode-map ("C-C C-l" . helm-comint-input-ring))
  :ensure nil)

(provide 'init-shell)
;;; init-shell.el ends here
