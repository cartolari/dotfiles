;;; init-uml.el -- uml inside emacs

;;; Commentary:
;;; edit and preview plant uml files inside Emacs
;;; Code:
(require 'use-package)

(use-package plantuml-mode
  :mode (("\\.puml\\'" . puml-mode)
         ("\\.plantuml\\'" . puml-mode)))

(provide 'init-uml)
;;; init-uml.el ends here
