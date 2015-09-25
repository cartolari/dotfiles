;;; init-display.el -- display setup

;;; Commentary:
;;; customize emacs display

;;; Code:
(require-package 'fill-column-indicator)
(require-package 'material-theme)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq indicate-buffer-boundaries
      '((t . right) (bottom . right)))
(setq indicate-empty-lines t)
(setq scroll-margin 3)
(setq scroll-step 1)
(setq-default scroll-preserver-screen-position t)

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message ";; *scratch*")

(set-face-attribute 'default nil :font "Source Code Pro for Powerline-12")

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-hook 'prog-mode-hook 'fci-mode)

(load-theme 'material t)

(provide 'init-display)
;;; init-display.el ends here
