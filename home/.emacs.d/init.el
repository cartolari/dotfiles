;; init.el --- cartolari emacs setup

;;; Commentary:
;;; my customizations and plugins

;;; Code:
(setq gc-cons-threshold 30000000)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

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
(setq use-package-always-ensure t)
(require 'use-package)

(use-package ag
  :commands (ag))
(use-package cssh
  :commands (cssh-term-remote-open))
(use-package dockerfile-mode
  :mode ("Dockerfile" . dockerfile-mode))
(use-package hydra)
(use-package key-seq
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.3))

(require 'init-display)
(require 'init-general-editing)
(require 'init-isearch)

(require 'init-company)
(require 'init-evil)
(require 'init-flycheck)
(require 'init-multiple-cursors)
(require 'init-powerline)

(require 'init-projectile)

(require 'init-clojure)
(require 'init-coffee)
(require 'init-haml)
(require 'init-ido)
(require 'init-java)
(require 'init-javascript)
(require 'init-lisp)
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
;; (require 'init-helm)

;; Emacs options
(setq gc-cons-threshold 20000000)
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
;; ;;; init.el ends here
