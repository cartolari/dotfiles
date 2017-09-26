;;; init-terraform.el -- Terraform mode setup

;;; Commentary:
;;; Terraform mode setup

;;; Code:
(use-package terraform-mode
  :mode (("\\.tf\\'" . terraform-mode)
         ("\\.tfvars\\'" . terraform-mode))
  :config
  (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode))

(use-package company-terraform
  :commands (company-terraform-init)
  :init
  (add-hook 'terraform-mode-hook 'company-terraform-init))

(provide 'init-terraform)
;;; init-terraform.el ends here
