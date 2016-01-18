;;; init-haml.el -- add haml support to emacs

;;; Commentary:
;;; install haml-mode

;;; Code:
(require 'use-package)

(use-package haml-mode
  :mode ("\\.haml\\'" . haml-mode))

(provide 'init-haml)
;;; init-haml.el ends here
