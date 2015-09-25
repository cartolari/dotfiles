;;; init-sh.el -- shell mode setup

;;; Commentary:
;;; shell mode setup

;;; Code:
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)
(add-hook 'sh-mode-hook
          (lambda()
            (setq evil-shift-width sh-indentation)))
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(provide 'init-sh)
;;; init-sh.el ends here
