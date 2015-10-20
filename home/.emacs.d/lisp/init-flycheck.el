;;; init-flycheck.el -- Flycheck setup

;;; Commentary:
;;; Load Flycheck and define custom checkers

;;; Code:
(use-package flycheck
  :config
  (flycheck-def-config-file-var flycheck-haml-lintrc haml-lint ".haml-lint.yml"
    :safe #'stringp)
  (flycheck-define-checker haml-lint
    "Haml style checker using haml-lint
See URL https://github.com/brigade/haml-lint"
    :command ("~/haml-lint.sh"
              (config-file "--config" flycheck-haml-lintrc)
              source)
    :error-patterns
    ((warning line-start
              (file-name) ":" line " [W] "  (message)
              line-end))
    :modes (haml-mode))
  (add-to-list 'flycheck-checkers 'haml-lint)
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :diminish flycheck-mode)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
