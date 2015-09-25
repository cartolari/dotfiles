;;; init-spell.el -- spell checking setup for Emacs

;;; Commentary:
;;; flyspell setup for emacs

;;; Code:
(require-package 'flyspell-popup)

(require 'flyspell)

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

(diminish 'flyspell-mode " Ⓢ")

(provide 'init-spell)
;;; init-spell.el ends here
