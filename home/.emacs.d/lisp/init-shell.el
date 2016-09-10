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
              (substitute-key-definition
               'eshell-pcomplete 'helm-esh-pcomplete eshell-mode-map)))
  :ensure nil)

(use-package shell
  :config
  (bind-keys :map shell-mode-map ("C-C C-l" . helm-comint-input-ring))
  :ensure nil)

(use-package eshell-fringe-status
  :commands (eshell-fringe-status-mode)
  :init
  (add-hook 'eshell-mode-hook 'eshell-fringe-status-mode))

(use-package eshell-did-you-mean
  :commands (eshell-did-you-mean-setup)
  :init
  (add-hook 'eshell-mode-hook 'eshell-did-you-mean-setup))


(use-package eshell-prompt-extras
  :init
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))

(provide 'init-shell)
;;; init-shell.el ends here
