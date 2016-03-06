;;; init-vc.el -- init version control setup

;;; Commentary:
;;; git setup

;;; Code:
(require 'use-package)

(use-package diff-hl
  :commands (diff-hl-dired-mode
             diff-hl-margin-mode
             diff-hl-amend-mode
             diff-hl-flydiff-mode)
  :config
  (set-face-attribute 'diff-hl-change nil
                      :foreground "blue3"
                      :background "#5656ff")
  (set-face-attribute 'diff-hl-delete nil
                      :foreground "red"
                      :background "red")
  (set-face-attribute 'diff-hl-insert nil
                      :foreground "#098d07"
                      :background "#ddffdf")
  (setq diff-hl-side 'right)
  :init
  (add-hook 'prog-mode-hook 'diff-hl-mode)
  (add-hook 'prog-mode-hook 'diff-hl-flydiff-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'dired-mode-hook 'diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package git-timemachine
  :commands (git-timemachine))

(use-package gitattributes-mode)

(use-package gitconfig-mode)

(use-package gitignore-mode)

(use-package magit
  :bind ("C-x g" . magit-status)
  :ensure nil
  :load-path "/home/bruno/code/magit/lisp")

(provide 'init-vc)
;;; init-vc.el ends here
