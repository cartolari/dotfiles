;;; init-typescript.el -- TypeScript setup

;;; Commentary:
;;; TypeScript language support

;;; Code:
(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2)
  :init
  (add-hook 'typescrit-mode 'tide-setup))

(use-package tide
  :config
  (tide-hl-identifier-mode 1)
  :commands (tide-setup))

(provide 'init-typescript)
;;; init-typescript.el ends here
