;;; init-javascript.el -- javascript setup

;;; Commentary:
;;; js2 mode setup and related stuff

;;; Code:
(require-package 'js2-mode)

(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(provide 'init-javascript)
;;; init-javascript.el ends here
