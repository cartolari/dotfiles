;;; init-helm.el --- Helm setup

;;; Commentary:
;;; configs and integrations for helm

;;; Code:
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring))
  :config
  (helm-mode 1)
  (setq helm-M-x-fuzzy-match t)
  :diminish helm-mode)
(use-package helm-projectile
  :defer t
  :init
  (add-hook 'projectile-mode-hook #'helm-projectile-on))

(provide 'init-helm)
;;; init-helm.el ends here
