;;; init-terraform.el -- Terraform mode setup

;;; Commentary:
;;; Terraform mode setup

;;; Code:
(use-package terraform-mode
  :mode (("\\.tf\\'" . terraform-mode)
         ("\\.tfvars\\'" . terraform-mode))
  :config
  (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode))

(provide 'init-terraform)
;;; init-terraform.el ends here
