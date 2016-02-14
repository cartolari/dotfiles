;;; init-coffee.el -- coffeescript setup

;;; Commentary:
;;; Coffee and related stuff

;;; Code:
(require 'use-package)

(defun my/coffee-evil-previousline-and-indent ()
  "Fix 'coffee-newline-and-indent' with 'evil-mode'."
  (interactive)
  (forward-line -1)
  (my/coffee-evil-newline-and-indent))

(defun my/coffee-evil-newline-and-indent ()
  "Fix 'coffee-newline-and-indent' with 'evil-mode'."
  (interactive)
  (end-of-line)
  (coffee-newline-and-indent)
  (evil-insert-state))

(use-package coffee-mode
  :config
  (evil-define-key 'normal coffee-mode-map "o" 'my/coffee-evil-newline-and-indent)
  (evil-define-key 'normal coffee-mode-map "O" 'my/coffee-evil-previousline-and-indent)
  (evil-define-key 'insert coffee-mode-map "RET" 'my/coffee-evil-newline-and-indent)
  :mode ("\\.coffee\\'" . coffee-mode))

(provide 'init-coffee)
;;; init-coffee.el ends here
