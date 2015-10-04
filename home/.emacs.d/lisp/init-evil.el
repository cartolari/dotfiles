;;; init-evil.el -- Evil mode setup

;;; Commentary:
;;; Some customization, mainly for letting evil mode to be more like vim.

;;; Code:

(require-package 'evil)
(require-package 'evil-anzu)
(require-package 'evil-commentary)
(require-package 'evil-exchange)
(require-package 'evil-exchange)
(require-package 'evil-indent-textobject)
(require-package 'evil-jumper)
(require-package 'evil-leader)
(require-package 'evil-matchit)
(require-package 'evil-surround)
(require-package 'evil-visualstar)

(require 'evil-commentary)
(require 'evil-exchange)
(require 'evil-jumper)
(require 'evil-leader)
(require 'evil-matchit)
(require 'evil-surround)
(require 'evil-visualstar)

(evil-commentary-mode 1)
(global-evil-jumper-mode 1)
(global-evil-leader-mode)
(global-evil-matchit-mode 1)
(global-evil-surround-mode 1)
(global-evil-visualstar-mode t)

;; Swap ; and : in evil mode
(define-key evil-normal-state-map (kbd ":") 'evil-repeat-find-char)
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-visual-state-map (kbd ";") 'evil-ex)

;; save file with ,s
(key-chord-define evil-insert-state-map ",s" (lambda ()
                                               (interactive)
                                               (progn
                                                 (save-buffer)
                                                 (evil-normal-state))))
(key-chord-define evil-normal-state-map ",s" 'save-buffer)

;; fuzzy find in current project with ,t
(key-chord-define evil-normal-state-map ",t"
                  (lambda ()
                    (interactive)
                    (cond ((projectile-project-p) (helm-projectile-find-file))
                          (t (helm-projectile-find-file-in-known-projects)))))

;; jk to exit insert mode in evil-mode
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; ,q to close the current window in evil-mode
(key-chord-define evil-normal-state-map ",q" 'delete-window)

;; enable evil-exchange only in visual mode
(key-chord-define evil-visual-state-map "cx" 'evil-exchange)

;; indent entire file with ==
(key-chord-define evil-normal-state-map (kbd "==")
  (lambda ()
    (interactive)
    (indent-region (point-min) (point-max))))

(add-hook 'prog-mode-hook 'evil-local-mode)
(add-hook 'find-file-hook 'evil-local-mode)

;; toggle evil-mode with C-z
(global-set-key (kbd "C-z") 'evil-local-mode)

(setq evil-exchange-key (kbd "gx"))

(setq-default evil-shift-width 2)

(diminish 'evil-commentary-mode)

(provide 'init-evil)
;;; init-evil.el ends here
