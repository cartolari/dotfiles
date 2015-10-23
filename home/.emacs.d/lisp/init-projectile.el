;;; init-projectile.el -- projetile setup

;;; Commentary:
;;; load projectile and customizations

;;; Code:
(use-package projectile
  :config
  (projectile-global-mode 1)
  (define-key projectile-mode-map (kbd "C-c p R") 'my/projectile-regenerate-tags)
  (key-seq-define evil-normal-state-map ",t" 'helm-projectile)
  :diminish projectile-mode)

(defun my/projectile-regenerate-tags ()
  "Regenerate projectile tags using git ctags."
  (interactive)
  (progn
    (start-process-shell-command
     "tags" nil (format "cd %s; git ctags"
                        (projectile-project-root)))
    (message "Generating tags file")))

(provide 'init-projectile)
;;; init-projectile.el ends here
