;;; init.el --- cartolari emacs setup

;;; Commentary:
;;; my customizations and plugins

;;; Code:
(setq gc-cons-threshold 30000000)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; (package-initialize)
(require 'init-elpa)
(require 'init-byte-compilation)

(require-package 'ag)
(require-package 'cssh)
(require-package 'diminish)
(require-package 'dockerfile-mode)
(require-package 'key-chord)
(require-package 'readline-complete)

(require 'key-chord)

(require 'init-display)
(require 'init-general-editing)
(require 'init-isearch)

(require 'init-company)
(require 'init-evil)
(require 'init-flycheck)
(require 'init-multiple-cursors)
(require 'init-powerline)

(require 'init-projectile)

(require 'init-coffee)
(require 'init-elisp)
(require 'init-haml)
(require 'init-helm)
(require 'init-java)
(require 'init-javascript)
(require 'init-markdown)
(require 'init-rest-client)
(require 'init-ruby)
(require 'init-sh)
(require 'init-spell)
(require 'init-tags)
(require 'init-uml)
(require 'init-vc)
(require 'init-web)
(require 'init-yaml)
(require 'init-yasnippet)

;; Emacs options
(setq gc-cons-threshold 20000000)
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
