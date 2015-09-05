;;; init-isearch.el -- isearch setup

;;; Commentary:
;;; use anzu instead of isearch

;;; Code:
(require-package 'anzu)

(require 'anzu)

(global-anzu-mode 1)

(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(diminish 'anzu-mode)

(provide 'init-isearch)
;;; init-isearch.el ends here
