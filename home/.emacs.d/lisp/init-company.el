;;; init-company.el -- Company mode setup

;;; Commentary:
;;; company setup

;;; Code:
(use-package company
  :config
  (global-company-mode)
  (bind-keys :map company-active-map
             ("TAB" . company-select-next)
             ([tab] . company-select-next)
             ([backtab] . company-select-previous)
             ("<backtab>" . company-select-previous)
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous))
  (add-hook 'company-completion-cancelled-hook (lambda (arg) (fci-mode 1)))
  (add-hook 'company-completion-finished-hook (lambda (arg) (fci-mode 1)))
  (add-hook 'company-completion-started-hook (lambda (arg) (fci-mode 0)))
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-idle-delay 0.25
        company-minimum-prefix-length 0)
  :diminish company-mode)
(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))
(use-package company-try-hard
  :defer t
  :init
  (autoload 'company-try-hard "company-try-hard" nil t)
  (global-set-key (kbd "C-'") #'company-try-hard))
(use-package company-ycmd
  :defer t
  :config
  :diminish ycmd-mode
  :init
  (add-hook 'after-init-hook (lambda ()
                               (progn
                                 (global-ycmd-mode)
                                 (company-ycmd-setup)
                                 (setq ycmd--log-enabled t))))
  (setq ycmd-server-command '("python2" "/home/bruno/code/ycmd/ycmd")))
(use-package readline-complete)

(global-set-key (kbd "M-/") 'hippie-expand)

(provide 'init-company)
;;; init-company.el ends here
