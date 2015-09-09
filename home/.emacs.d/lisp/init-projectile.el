;;; init-projectile.el -- projetile setup

;;; Commentary:
;;; load projectile and customizations

;;; Code:
(require-package 'projectile)

(require 'projectile)

(projectile-global-mode 1)

(setq projectile-completion-system 'helm
      projectile-switch-project-action 'helm-projectile)

(diminish 'projectile-mode " Ⓟ")

(provide 'init-projectile)
;;; init-projectile.el ends here
