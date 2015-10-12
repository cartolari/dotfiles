;;; init-uml.el -- uml inside emacs

;;; Commentary:
;;; edit and preview plant uml files inside Emacs
;;; Code:
(use-package puml-mode
  :mode (("\\.puml\\'" . puml-mode)
         ("\\.plantuml\\'" . puml-mode)))

(provide 'init-uml)
;;; init-uml.el ends here
