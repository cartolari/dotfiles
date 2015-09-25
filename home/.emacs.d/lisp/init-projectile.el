;;; init-projectile.el -- projetile setup

;;; Commentary:
;;; load projectile and customizations

;;; Code:
(require-package 'projectile)

(require 'projectile)

(projectile-global-mode 1)

(setq projectile-completion-system 'helm
      projectile-switch-project-action 'helm-projectile)

(define-key projectile-mode-map
  (kbd "C-c p R") (lambda ()
                    (interactive)
                    (progn
                      (start-process-shell-command
                       "tags" nil (format "cd %s; git ctags"
                                          (projectile-project-root)))
                      (message "Generating tags file"))))

(diminish 'projectile-mode " Ⓟ")

(provide 'init-projectile)
;;; init-projectile.el ends here
