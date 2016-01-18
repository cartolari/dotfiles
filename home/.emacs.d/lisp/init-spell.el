;;; init-spell.el -- spell checking setup for Emacs

;;; Commentary:
;;; flyspell setup for emacs

;;; Code:
(require 'use-package)

(use-package flyspell
  :ensure nil
  :diminish flyspell-mode
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode))

(provide 'init-spell)
;;; init-spell.el ends here
