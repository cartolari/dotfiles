;;; init-ido.el -- Ido mode setup

;;; Commentary:
;;; Ido and related packages

;;; Code:
(use-package ido
  :defer t
  :ensure nil
  :config
  (ido-mode 1)
  :init
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-use-filename-at-point 'guess
        ido-create-new-buffer 'always)
  (add-hook 'after-init-hook #'ido-mode))

(use-package ido-describe-bindings
  :commands (ido-describe-bindings))

(use-package ido-ubiquitous
  :commands (ido-ubiquitous-mode)
  :init
  (add-hook 'after-init-hook #'ido-ubiquitous-mode))

(use-package ido-vertical-mode
  :commands (ido-vertical-mode)
  :config
  (setq ido-vertical-show-count t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :init
  (add-hook 'after-init-hook #'ido-vertical-mode))

(use-package flx-ido
  :commands (flx-ido-mode)
  :config
  (setq ido-use-faces nil)
  :init
  (add-hook 'after-init-hook #'flx-ido-mode))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (smex-initialize))

(provide 'init-ido)
;;; init-ido.el ends here
