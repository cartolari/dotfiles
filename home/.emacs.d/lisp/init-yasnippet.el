;;; init-yasnippet.el -- yasnippet setup

;;; Commentary:
;;; yasnippet and custom snippets

;;; Code:
(require 'use-package)

(use-package yasnippet
  :commands (yas-global-mode)
  :init
  (add-hook 'after-init-hook 'yas-global-mode)
  :diminish yas-minor-mode)

(use-package yasnippet-vim-snippets
  :load-path "~/code/yasnippet-vim-snippets"
  :ensure nil)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
