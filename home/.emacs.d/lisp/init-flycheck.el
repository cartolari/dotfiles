;;; init-flycheck.el -- Flycheck setup

;;; Commentary:
;;; Load Flycheck and define custom checkers

;;; Code:
(require 'use-package)

(use-package flycheck
  :commands (global-flycheck-mode)
  :config
  (flycheck-def-config-file-var flycheck-haml-lintrc haml-lint ".haml-lint.yml" :safe #'stringp)
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
  (bind-key "s-f" (defhydra flycheck-hydra ()
                    "errors"
                    ("n" flycheck-next-error "next")
                    ("p" flycheck-previous-error "previous")
                    ("q" nil "quit"))
            flycheck-mode-map)
  (define-fringe-bitmap 'circle-fringe-indicator
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))

  (dolist (error-level '(error warning info))
    (flycheck-define-error-level error-level
      :overlay-category (intern (format "flycheck-%s-overlay" error-level))
      :fringe-bitmap 'circle-fringe-indicator
      :fringe-face (intern (format "flycheck-fringe-%s" error-level))))

  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :diminish flycheck-mode)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
