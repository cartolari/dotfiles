;; init.el --- cartolari emacs setup

;;; Commentary:
;;; my customizations and plugins

;;; Code:
(setq gc-cons-threshold (* 800000 20))
(setq gc-cons-percentage 0.2)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package dockerfile-mode
  :mode ("Dockerfile" . dockerfile-mode))
(use-package hydra)

(require 'init-macros)

(require 'init-display)
(require 'init-general-editing)
(require 'init-isearch)

(require 'init-completion)
(require 'init-evil)
(require 'init-flycheck)
(require 'init-multiple-cursors)
(require 'init-powerline)

(require 'init-projectile)

(require 'init-clojure)
(require 'init-coffee)
(require 'init-folding)
(require 'init-go)
(require 'init-haml)
(require 'init-helm)
(require 'init-java)
(require 'init-javascript)
(require 'init-lisp)
(require 'init-markdown)
(require 'init-rest-client)
(require 'init-ruby)
(require 'init-sh)
(require 'init-shell)
(require 'init-spell)
(require 'init-tags)
(require 'init-terraform)
(require 'init-uml)
(require 'init-vc)
(require 'init-vim)
(require 'init-web)
(require 'init-yaml)
(require 'init-yasnippet)

;; Emacs options
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
