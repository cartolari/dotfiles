;;; init-isearch.el -- isearch setup

;;; Commentary:
;;; use anzu instead of isearch

;;; Code:
(require 'use-package)

(use-package anzu
  :bind (("C-M-%" . anzu-query-replace-regexp)
         ("M-%" . anzu-query-replace)
         ("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp))
  :defer t
  :diminish anzu-mode
  :init
  (global-anzu-mode 1))

(use-package flx-isearch
  :bind (("C-s" . my/isearch-forward)
         ("C-r " . my/isearch-backward))
  :config
  (defun my/isearch-forward (&optional regexp-p no-recursive-edit)
    (interactive "P\np")
    (cond ((equal current-prefix-arg nil) (flx-isearch-forward))
          ((equal current-prefix-arg '(4)) (isearch-forward-regexp))
          (t (isearch-forward))))
  (defun my/isearch-backward (&optional regexp-p no-recursive-edit)
    (interactive "P\np")
    (cond ((equal current-prefix-arg nil) (flx-isearch-backward))
          ((equal current-prefix-arg '(4)) (isearch-backward-regexp))
          (t (isearch-backward)))))

(provide 'init-isearch)
;;; init-isearch.el ends here
