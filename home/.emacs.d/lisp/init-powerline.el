;;; init-powerline.el -- Powerline configuration

;;; Commentary:
;;; Try to mimic Spacemacs modeline

;;; Code:
(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (setq powerline-default-separator 'wave)
  (setq powerline-height 25)
  (setq anzu-cons-mode-line-p nil)
  (setq anzu-mode-line-update-function
        (lambda (here total)
          (propertize (format "<%d/%d>" here total)
                      'face (spaceline-highlight-face-default))))
  (set-face-attribute 'mode-line nil :font "Source Code Pro for Powerline-10")
  (set-face-attribute (spaceline-highlight-face-default) nil :weight 'bold)
  (spaceline-install
   '(((anzu evil-state) :face highlight-face)
     (buffer-modified buffer-size buffer-id remote-host)
     major-mode
     ((flycheck-error flycheck-warning flycheck-info) :when active)
     (((minor-modes :separator " ") process) :when active)
     (version-control :when active))

   `(selection-info
     ((buffer-encoding-abbrev point-position line-column) :separator " | ")
     (global :when active)
     buffer-position
     hud)))

(defadvice vc-mode-line (after strip-backend () activate)
  "Remove the Git string from the 'vc-mode-line'."
  (when (stringp vc-mode)
    (let ((noback (replace-regexp-in-string
                   (format "^ %s\\(:\\|-\\)?" (vc-backend buffer-file-name))
                   " " vc-mode)))
      (setq vc-mode noback))))

(provide 'init-powerline)
;;; init-powerline.el ends here
