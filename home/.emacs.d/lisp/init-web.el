;;; init-web.el -- setup web stuff

;;; Commentary:
;;; setup web development related stuff

;;; Code:
(require-package 'emmet-mode)
(require-package 'rainbow-mode)
(require-package 'scss-mode)
(require-package 'web-mode)

(require 'emmet-mode)
(require 'rainbow-mode)
(require 'scss-mode)
(require 'web-mode)

(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)

(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes

(setq-default css-indent-offset 2)

(diminish 'emmet-mode)
(diminish 'rainbow-mode " 🌈")

(provide 'init-web)
;;; init-web.el ends here
