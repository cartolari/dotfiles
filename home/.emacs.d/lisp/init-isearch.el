;;; init-isearch.el -- Search setup

;;; Commentary:
;;; Setup plugins for using with Emacs and external search

;;; Code:
(require 'use-package)

(use-package ag)

(use-package anzu
  :bind (("C-M-%" . anzu-query-replace-regexp)
         ("M-%" . anzu-query-replace)
         ("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp))
  :defer t
  :diminish anzu-mode
  :init
  (global-anzu-mode 1))

(provide 'init-isearch)
;;; init-isearch.el ends here
