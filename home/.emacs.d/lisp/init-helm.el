;;; init-helm.el --- Helm setup

;;; Commentary:
;;; configs and integrations for helm

;;; Code:
(use-package shackle
  :config
  (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.30)))
  (shackle-mode)
  :diminish shackle-mode)

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring))
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
  :diminish helm-mode
  :demand t)

(use-package helm-ag)

(use-package helm-projectile
  :defer t
  :init
  (add-hook 'projectile-mode-hook #'helm-projectile-on)
  (setq projectile-completion-system 'helm))

(provide 'init-helm)
;;; init-helm.el ends here
