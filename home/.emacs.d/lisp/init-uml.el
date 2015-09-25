;;; init-uml.el -- uml inside emacs

;;; Commentary:
;;; edit and preview plant uml files inside Emacs
;;; Code:
(require-package 'puml-mode)

(add-to-list 'auto-mode-alist
             '("\\.puml\\'" . puml-mode)
             '("\\.plantuml\\'" . puml-mode))

(provide 'init-uml)
;;; init-uml.el ends here
