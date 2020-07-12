;;; init-projectile.el -- projetile setup

;;; Commentary:
;;; load projectile and customizations

;;; Code:
(require 'use-package)

(use-package projectile
  :commands (projectile-global-mode)
  :config
  (projectile-global-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p R") 'my/projectile-regenerate-tags)
  (with-eval-after-load 'evil
    (key-seq-define evil-normal-state-map ",t" 'helm-projectile))
  (setq projectile-switch-project-action 'projectile-commander)
  (def-projectile-commander-method ?s
    "Open a *shell* buffer for the project."
    (projectile-run-shell))
  (def-projectile-commander-method ?d
    "Open project root in dired."
    (projectile-dired))
  :diminish projectile-mode
  :init
  (add-hook 'after-init-hook 'projectile-global-mode))

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
