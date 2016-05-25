;;; init-powerline.el -- Powerline configuration

;;; Commentary:
;;; Simplified mode line setup that focuses on providing only the most useful
;;; information

;;; Code:
(require 'use-package)

(use-package eyebrowse
  :commands (eyebrowse-mode)
  :config
  (setq eyebrowse-new-workspace t)
  (bind-key "C-c w" (defhydra hydra-eyebrowse ()
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
  (global-set-key (kbd "M-<left>") 'eyebrowse-prev-window-config)
  (global-set-key (kbd "M-<right>") 'eyebrowse-next-window-config)
  :init
  (add-hook 'after-init-hook 'eyebrowse-mode))

(use-package window-numbering
  :commands (window-numbering-mode)
  :config
  (defun window-numbering-install-mode-line (&optional ignored))
  :init
  (add-hook 'after-init-hook 'window-numbering-mode))

(use-package spaceline
  :init
  (add-hook
   'after-init-hook
   (lambda ()
     (progn
       (require 'spaceline-segments)
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
          hud))
       (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))))))

(defadvice vc-mode-line (after strip-backend () activate)
  "Remove the Git string from the 'vc-mode-line'."
  (when (stringp vc-mode)
    (let ((noback (replace-regexp-in-string
                   (format "^ %s\\(:\\|-\\)?" (vc-backend buffer-file-name))
                   " " vc-mode)))
      (setq vc-mode noback))))

(provide 'init-powerline)
;;; init-powerline.el ends here
