;;; init-folding.el -- Folding Setup

;;; Commentary:
;;; Folding Setup

;;; Code:
(use-package yafolding
  :commands (yafolding-mode)
  :init
  (add-hook 'prog-mode-hook 'yafolding-mode))

(provide 'init-folding)
;;; init-folding.el ends here
