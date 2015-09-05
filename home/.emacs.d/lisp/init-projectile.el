;;; init-projectile.el -- projetile setup

;;; Commentary:
;;; load projectile and customizations

;;; Code:
(require-package 'projectile)

(require 'projectile)

(projectile-global-mode 1)

(diminish 'projectile-mode " ℗ ")

(provide 'init-projectile)
;;; init-projectile.el ends here
