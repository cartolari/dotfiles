;;; init-helm.el --- Helm setup

;;; Commentary:
;;; configs and integrations for helm

;;; Code:
(require-package 'helm)
(require-package 'helm-projectile)

(require 'helm)
(require 'helm-projectile)

(helm-mode 1)
(helm-projectile-on)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(setq helm-M-x-fuzzy-match t)

(diminish 'helm-mode)

(provide 'init-helm)
;;; init-helm.el ends here
