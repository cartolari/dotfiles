(require-package 'evil)
(require-package 'evil-anzu)
(require-package 'evil-commentary)
(require-package 'evil-exchange)
(require-package 'evil-jumper)
(require-package 'evil-leader)
(require-package 'evil-matchit)
(require-package 'evil-surround)
(require-package 'evil-visualstar)

(require 'evil-commentary)
(require 'evil-jumper)
(require 'evil-matchit)
(require 'evil-leader)
(require 'evil-surround)

(evil-commentary-mode 1)
(global-evil-jumper-mode 1)
(global-evil-matchit-mode 1)
(global-evil-surround-mode 1)
(global-evil-leader-mode)

(define-key evil-normal-state-map (kbd ":") 'evil-repeat-find-char)
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-visual-state-map (kbd ";") 'evil-ex)

(key-chord-define evil-insert-state-map ",s" (lambda ()
                                               (interactive)
                                               (progn
                                                 (save-buffer)
                                                 (evil-normal-state))))
(key-chord-define evil-normal-state-map ",s" 'save-buffer)

(key-chord-define evil-normal-state-map ",t" 'helm-projectile-find-file)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-normal-state-map ",q" 'delete-window)

(add-hook 'prog-mode-hook 'evil-local-mode)
(add-hook 'yaml-mode-hook 'evil-local-mode)
(add-hook 'snippet-mode 'evil-local-mode)

(global-set-key (kbd "C-z") 'evil-local-mode)

(diminish 'evil-commentary-mode)

(provide 'init-evil)
