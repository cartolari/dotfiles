;;; init-display.el -- display setup

;;; Commentary:
;;; customize emacs display

;;; Code:
(require 'use-package)

(use-package fill-column-indicator
  :commands (fci-mode)
  :init
  (add-hook 'prog-mode-hook 'fci-mode))

(use-package material-theme
  :init
  (load-theme 'material t))

(use-package linum
  :commands (global-linum-mode)
  :config
  (defvar linum-relativenumber-last-pos 0 "Internally used for relative linum.")
  (defvar linum-format)
  (defvar linum-last-pos)
  (defadvice linum-update (before linum-relativenumber-linum-update activate)
    (setq linum-last-pos (line-number-at-pos)))
  (defun linum-relativenumber-format (line-number)
    (let* ((diff (abs (- line-number linum-last-pos)))
           (current-line-p (zerop diff))
           (number (if current-line-p (line-number-at-pos) diff)))
      (propertize (format "%3d" number)
                  'face
                  (if current-line-p 'highlight
                    'linum))))
  (setq linum-format 'linum-relativenumber-format)
  :ensure nil
  :init
  (add-hook 'after-init-hook 'global-linum-mode))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq scroll-margin 3)
(setq scroll-step 1)
(setq-default scroll-preserver-screen-position t)

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message ";; *scratch*")

(set-face-attribute 'default nil :font "Source Code Pro for Powerline-12")

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'init-display)
;;; init-display.el ends here
