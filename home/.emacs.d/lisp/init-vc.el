;;; init-vc.el -- init version control setup

;;; Commentary:
;;; git setup

;;; Code:
(require-package 'git-timemachine)
(require-package 'gitattributes-mode)
(require-package 'gitconfig-mode)
(require-package 'gitignore-mode)
(require-package 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

(provide 'init-vc)
;;; init-vc.el ends here
