;;; init-yasnippet.el -- yasnippet setup

;;; Commentary:
;;; yasnippet and custom snippets

;;; Code:
(require-package 'yasnippet)

(require 'yasnippet)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets" "~/code/yasnippet-snippets"))

(yas-global-mode 1)

(diminish 'yas-minor-mode)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
