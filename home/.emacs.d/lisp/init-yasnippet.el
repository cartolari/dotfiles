;;; init-yasnippet.el -- yasnippet setup

;;; Commentary:
;;; yasnippet and custom snippets

;;; Code:
(require-package 'yasnippet)

(require 'yasnippet)

(yas-global-mode 1)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets" "~/code/yasnippet-snippets"))

(diminish 'yas-minor-mode)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
