;;; init-projectile.el -- projetile setup

;;; Commentary:
;;; load projectile and customizations

;;; Code:
(use-package projectile
  :config
  (projectile-global-mode 1)
  (define-key projectile-mode-map (kbd "C-c p R") 'my/projectile-regenerate-tags)
  ;; fuzzy find in current project with ,t
  (key-seq-define evil-normal-state-map ",t" 'projectile-find-in-current-or-all-projects)
  :diminish projectile-mode)

(defun my/projectile-regenerate-tags ()
  "Regenerate projectile tags using git ctags."
  (interactive)
  (progn
    (start-process-shell-command
     "tags" nil (format "cd %s; git ctags"
                        (projectile-project-root)))
    (message "Generating tags file")))

(defun projectile-find-in-current-or-all-projects ()
  "Find a file in current (if any) or all projectile projects."
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (projectile-find-file-in-known-projects)))

(provide 'init-projectile)
;;; init-projectile.el ends here
