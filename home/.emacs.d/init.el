;;; init.el --- cartolari emacs setup

;;; Commentary:
;;; my customizations and plugins

;;; Code:
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

(require 'init-elpa)

(require-package 'diminish)
(require-package 'flyspell-popup)
(require-package 'key-chord)
(require-package 'material-theme)

(require 'key-chord)

(require 'init-general-editing)
(require 'init-isearch)
(require 'init-display)

(require 'init-company)
(require 'init-evil)
(require 'init-flycheck)
(require 'init-multiple-cursors)
(require 'init-powerline)

(require 'init-projectile)

(require 'init-haml)
(require 'init-helm)
(require 'init-ruby)
(require 'init-yaml)
(require 'init-yasnippet)

(require-package 'ace-window)
(require-package 'ag)
(require-package 'avy)
(require-package 'cssh)
(require-package 'magit)
(require-package 'readline-complete)

(add-hook 'emacs-lisp-mode-hook
          (lambda()
            (setq mode-name "Elisp")))

;; Mode hooks
(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2
                                  tab-width 2)))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq evil-shift-width python-indent))))

(load-theme 'material t)

;; Emacs options
(setq gc-cons-threshold 20000000)
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
