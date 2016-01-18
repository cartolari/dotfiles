;;; init-java.el -- Java setup

;;; Commentary:
;;; Java and eclim setup

;;; Code:
(require 'use-package)

(use-package emacs-eclim
  :commands (global-eclim-mode)
  :config
  (global-eclim-mode 1)
  (company-emacs-eclim-setup)
  (setq eclim-eclipse-dirs "/opt/eclipse")
  (setq eclim-executable "/opt/eclipse/eclim")
  (setq eclimd-default-workspace "~/eclipse_workspace")
  (setq eclimd-executable "/opt/eclipse/eclimd")
  (setq company-emacs-eclim-ignore-case t)
  :init
  (add-hook 'java-mode-hook #'global-eclim-mode))

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2
                                  tab-width 2)))

(provide 'init-java)
;;; init-java.el ends here
