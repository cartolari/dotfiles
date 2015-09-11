;;; init-java.el -- Java setup

;;; Commentary:
;;; Java and eclim setup

;;; Code:
(require-package 'emacs-eclim)

(require 'eclim)
(require 'eclimd)
(require 'company-emacs-eclim)

(global-eclim-mode 1)
(company-emacs-eclim-setup)

(setq eclim-eclipse-dirs "/opt/eclipse")
(setq eclim-executable "/opt/eclipse/eclim")
(setq eclimd-default-workspace "~/eclipse_workspace")
(setq eclimd-executable "/opt/eclipse/eclimd")

(setq company-emacs-eclim-ignore-case t)

(provide 'init-java)
;;; init-java.el ends here
