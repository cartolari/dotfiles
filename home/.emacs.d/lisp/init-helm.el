;;; init-helm.el --- Helm setup

;;; Commentary:
;;; configs and integrations for helm

;;; Code:
(require 'use-package)

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-h f" . helm-apropos)
         ("C-h r" . helm-info-emacs)
         ("C-h C-l" . helm-locate-library))
  :config
  (helm-mode 1)
  (bind-keys :map minibuffer-local-map ("C-C C-l" . helm-minibuffer-history))
  (setq helm-display-function 'pop-to-buffer
        helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-recentf-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-locate-fuzzy-match t
        helm-M-x-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-lisp-fuzzy-completion t)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.3)))
  :diminish helm-mode)

(use-package helm-ag
  :commands (helm-ag
             helm-do-ag
             helm-ag-project-root
             helm-do-ag-project-root))

(use-package helm-dash
  :commands (helm-dash helm-dash-at-point)
  :config
  (setq helm-dash-common-docsets (helm-dash-installed-docsets)
        helm-dash-browser-func 'eww)
  :init
  (with-eval-after-load 'evil
    (bind-keys :map evil-normal-state-map ("K" . helm-dash-at-point))))

(use-package helm-descbinds
  :commands (helm-descbinds-mode)
  :init
  (add-hook 'after-init-hook 'helm-descbinds-mode))

(use-package helm-flx
  :commands (helm-flx-mode)
  :init
  (add-hook 'after-init-hook 'helm-flx-mode))

(use-package helm-fuzzier
  :commands (helm-fuzzier-mode)
  :init
  (add-hook 'after-init-hook 'helm-fuzzier-mode))

(use-package helm-gtags
  :commands (helm-gtags-mode)
  :init
  (add-hook 'after-init-hook 'helm-gtags-mode))

(use-package helm-projectile
  :commands (helm-projectile-on)
  :init
  (add-hook 'projectile-mode-hook 'helm-projectile-on)
  (setq projectile-completion-system 'helm))

(provide 'init-helm)
;;; init-helm.el ends here
