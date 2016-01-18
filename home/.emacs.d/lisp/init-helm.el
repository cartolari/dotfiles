;;; init-helm.el --- Helm setup

;;; Commentary:
;;; configs and integrations for helm

;;; Code:
(require 'use-package)

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini))
  :config
  (helm-mode 1)
  (setq helm-display-function                 'pop-to-buffer
        helm-mode-fuzzy-match                 t
        helm-completion-in-region-fuzzy-match t
        helm-ff-search-library-in-sexp        t
        helm-scroll-amount                    8
        helm-ff-file-name-history-use-recentf t
        helm-recentf-fuzzy-match              t
        helm-buffers-fuzzy-matching           t
        helm-recentf-fuzzy-match              t
        helm-buffers-fuzzy-matching           t
        helm-locate-fuzzy-match               t
        helm-M-x-fuzzy-match                  t
        helm-semantic-fuzzy-match             t
        helm-imenu-fuzzy-match                t
        helm-apropos-fuzzy-match              t
        helm-lisp-fuzzy-completion            t)
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

(use-package helm-projectile
  :commands (helm-projectile-on)
  :init
  (add-hook 'projectile-mode-hook 'helm-projectile-on)
  (setq projectile-completion-system 'helm))

(provide 'init-helm)
;;; init-helm.el ends here
