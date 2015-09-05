(require-package 'flycheck)

(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

(diminish 'flycheck-mode " © ")
(provide 'init-flycheck)
