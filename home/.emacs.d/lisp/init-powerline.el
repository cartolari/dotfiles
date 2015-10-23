;;; init-powerline.el -- Powerline configuration

;;; Commentary:
;;; Try to mimic Spacemacs modeline

;;; Code:
(use-package eyebrowse
  :config
  (eyebrowse-mode)
  (setq eyebrowse-new-workspace t)
  (bind-key "s-w" (defhydra hydra-eyebrowse ()
                    "eyebrowse"
                    ("n" eyebrowse-next-window-config "next")
                    ("N" (lambda ()
                           (interactive)
                           (eyebrowse-switch-to-window-config
                            (+ 1 (eyebrowse--get 'current-slot))))
                     "new")
                    ("p" eyebrowse-prev-window-config "previous")
                    ("c" eyebrowse-close-window-config "close")
                    ("l" eyebrowse-last-window-config "last")
                    ("q" nil "quit"))
            eyebrowse-mode-map)
  :diminish eyebrowse-mode)

(use-package window-numbering
  :config (window-numbering-mode))

(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (setq anzu-cons-mode-line-p nil)
  (setq anzu-mode-line-update-function
        (lambda (here total)
          (propertize (format "<%d/%d>" here total)
                      'face (spaceline-highlight-face-default))))
  (setq powerline-default-separator 'wave)
  (setq powerline-height 25)
  (setq spaceline-window-numbers-unicode t)
  (setq spaceline-workspace-numbers-unicode t)
  (set-face-attribute 'mode-line nil :font "Source Code Pro for Powerline-10")
  (set-face-attribute (spaceline-highlight-face-default) nil :weight 'bold)
  (spaceline-install
   '(((anzu workspace-number window-number evil-state) :face highlight-face :separator " | ")
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
