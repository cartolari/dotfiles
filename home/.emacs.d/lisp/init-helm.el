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
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.3)))
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*swiper*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.25)))
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

(use-package helm-gtags
  :commands (helm-gtags-mode)
  :init
  (add-hook 'after-init-hook 'helm-gtags-mode))

(use-package helm-projectile
  :commands (helm-projectile-on)
  :config
  (defun helm-projectile-on ()
    "Override 'helm-projectile-on' to not show message when turned on."
    (helm-projectile-toggle 1))
  :init
  (add-hook 'projectile-mode-hook 'helm-projectile-on)
  (setq projectile-completion-system 'helm))

(use-package swiper-helm
  :bind (("C-s" . my/isearch-forward)
         ("C-r " . my/isearch-backward))
  :config
  (setq swiper-helm-display-function 'display-buffer)
  (defun my/isearch-forward (&optional regexp-p no-recursive-edit)
    (interactive "P\np")
    (cond ((equal current-prefix-arg nil)
           (if (minibufferp)
               (isearch-forward)
             (swiper-helm)))
          ((equal current-prefix-arg '(4)) (isearch-forward-regexp))
          (t (isearch-forward))))
  (defun my/isearch-backward (&optional regexp-p no-recursive-edit)
    (interactive "P\np")
    (cond ((equal current-prefix-arg nil)
           (if (minibufferp)
               (isearch-backward)
             (swiper-helm)))
          ((equal current-prefix-arg '(4)) (isearch-backward-regexp))
          (t (isearch-backward)))))

(provide 'init-helm)
;;; init-helm.el ends here
