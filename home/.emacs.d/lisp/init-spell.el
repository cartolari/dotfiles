;;; init-spell.el -- spell checking setup for Emacs

;;; Commentary:
;;; flyspell setup for emacs

;;; Code:
(use-package flyspell-popup
  :commands (flyspell-popup-correct))

(use-package flyspell
  :ensure nil
  :diminish " Ⓢ"
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode))

(provide 'init-spell)
;;; init-spell.el ends here
