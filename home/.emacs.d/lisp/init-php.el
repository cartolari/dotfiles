;;; init-php.el -- PHP language support

;;; Commentary:
;;; Add support for PHP language and tools

;;; Code:
(use-package php-mode
  :config
  (require 'geben)
  (require 'geben-helm-projectile)
  :mode ("\\.php\\'" . php-mode))

(use-package company-php
  :config
  (add-hook 'php-mode-hook
            '(lambda ()
               (add-to-list 'company-backends 'company-ac-php-backend))))

(use-package geben
  :config
  (defun geben-eval-at-point ()
    (interactive)
    (modify-syntax-entry ?$ "w" php-mode-syntax-table)
    (let ((symbol (if mark-active
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (thing-at-point 'symbol))))
      (geben-eval-expression symbol)))

  (defvar my/overlay nil)

  (defun my/remove-current-line-highlight ()
    (if my/overlay
        (progn
          (delete-overlay my/overlay)
          (setq my/overlay nil))))

  (defun my/highlight-lines-exclusively (line buffer)
    (my/remove-current-line-highlight)
    (setq my/overlay (geben-overlay-make-line line buffer))
    (overlay-put my/overlay 'face 'geben-breakpoint-face)
    (overlay-put my/overlay 'evaporate t)
    (overlay-put my/overlay 'modification-hooks '(geben-bp-overlay-modified))
    (overlay-put my/overlay 'insert-in-front-hooks '(geben-bp-overlay-inserted-in-front)))

  (defun geben-session-cursor-overlay-update (session)
    (let* ((cursor (geben-session-cursor session))
           (overlay (plist-get cursor :overlay))
           (position (plist-get cursor :position))
           (fileuri (car position))
           (lineno (cdr position))
           (local-path (and fileuri
                            (geben-session-source-local-path session fileuri))))
      (if (null position)
          (my/remove-current-line-highlight)
        (let ((buf (geben-source-visit local-path)))
          (when buf
            (with-current-buffer buf
              (ignore-errors
                (save-restriction
                  (widen)
                  (goto-line lineno)
                  (my/highlight-lines-exclusively lineno buf)))))))))

  (setq geben-display-window-function 'switch-to-buffer
        geben-show-redirect-buffers nil
        geben-show-breakpoints-debugging-only nil)

  (define-key geben-mode-map (kbd "C-c C-e") 'geben-eval-at-point)
  :defer t)

(use-package geben-helm-projectile
  :defer t)

(provide 'init-php)
;;; init-php.el ends here
