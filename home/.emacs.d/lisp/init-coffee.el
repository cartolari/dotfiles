;;; init-coffee.el -- coffeescript setup

;;; Commentary:
;;; Coffee and related stuff

;;; Code:
(require 'use-package)

(use-package coffee-mode
  :mode ("\\.coffee\\'" . coffee-mode))

(provide 'init-coffee)
;;; init-coffee.el ends here
