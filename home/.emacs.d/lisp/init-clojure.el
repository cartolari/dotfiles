;;; init-clojure.el -- Clojure support for emacs

;;; Commentary:
;;; setup cider and clojure-mode

;;; Code:
(require 'use-package)

(use-package clojure-mode
  :mode ("\\.clj\\'" . clojure-mode))

(use-package cider
  :config
  (setq cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-use-pretty-printing t
        cider-repl-use-clojure-font-lock t
        cider-repl-result-prefix ";; => "
        cider-repl-wrap-history t
        cider-repl-history-size 3000)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  :defer t
  :init
  (add-hook 'clojure-mode-hook 'cider-mode))

(use-package clj-refactor
  :defer t
  :config
  (cljr-add-keybindings-with-prefix "C-c C-r")
  :init
  (autoload 'clj-refactor-mode "clj-refactor")
  (add-hook 'clojure-mode-hook 'clj-refactor-mode))

(use-package flycheck-clojure
  :defer t
  :init
  (autoload 'flycheck-clojure-setup "flycheck-clojure")
  (add-hook 'clojure-mode 'flycheck-clojure-setup))

(provide 'init-clojure)
;;; init-clojure.el ends here
