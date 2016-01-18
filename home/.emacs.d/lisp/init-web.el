;;; init-web.el -- setup web stuff

;;; Commentary:
;;; setup web development related stuff

;;; Code:
(require 'use-package)

(use-package emmet-mode
  :commands (emmet-mode)
  :diminish emmet-mode
  :init
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode))
(use-package rainbow-mode
  :commands (rainbow-mode)
  :diminish rainbow-mode
  :init
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'scss-mode-hook 'rainbow-mode))
(use-package scss-mode
  :mode ("\\.scss\\'" . scss-mode))

(setq-default css-indent-offset 2)

(provide 'init-web)
;;; init-web.el ends here
