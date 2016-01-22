;;; init-helm.el --- Helm setup

;;; Commentary:
;;; configs and integrations for helm

;;; Code:
(require 'use-package)

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini))
  :config
  (helm-mode 1)
  (setq helm-display-function                 'pop-to-buffer)
  (defun my-helm-make-source (f &rest args)
    (nconc args '(:fuzzy-match t))
    (apply f args))
  (advice-add 'helm-make-source :around 'my-helm-make-source)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.3)))
  :diminish helm-mode)

(use-package helm-ag
  :commands (helm-ag
             helm-do-ag
             helm-ag-project-root
             helm-do-ag-project-root))

(use-package helm-flx
  :commands (helm-flx-mode)
  :init
  (add-hook 'after-init-hook 'helm-flx-mode))

(use-package helm-fuzzier
  :commands (helm-fuzzier-mode)
  :init
  (add-hook 'after-init-hook 'helm-fuzzier-mode))

(use-package helm-projectile
  :commands (helm-projectile-on)
  :init
  (add-hook 'projectile-mode-hook 'helm-projectile-on)
  (setq projectile-completion-system 'helm))

(provide 'init-helm)
;;; init-helm.el ends here
