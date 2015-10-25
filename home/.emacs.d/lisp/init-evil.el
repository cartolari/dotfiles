;;; init-evil.el -- Evil mode setup

;;; Commentary:
;;; Some customization, mainly for letting evil mode to be more like vim.

;;; Code:
(use-package evil
  :bind ("C-z" . evil-local-mode)
  :config
  (setq-default evil-shift-width 2)
  (setq evil-move-beyond-eol t)
  ;; Swap ; and : in evil mode
  (bind-keys :map evil-normal-state-map
             (":" . evil-repeat-find-char)
             (";" . evil-ex))
  (bind-keys :map evil-visual-state-map
             (":" . evil-repeat-find-char)
             (";" . evil-ex))
  ;; jk to exit insert mode in evil-mode
  (key-seq-define evil-insert-state-map "jk" 'evil-normal-state)
  ;; ,q to close the current window in evil-mode
  (key-seq-define evil-normal-state-map ",q" 'delete-window)
  ;; indent entire
  (key-seq-define evil-normal-state-map "==" (lambda ()
                                               (interactive)
                                               (indent-region
                                                (point-min) (point-max))))
  ;; save file with ,s
  (key-seq-define evil-insert-state-map ",s" (lambda ()
                                               (interactive)
                                               (progn
                                                 (evil-normal-state)
                                                 (save-buffer))))
  (key-seq-define evil-normal-state-map ",s" 'save-buffer)
  :init
  (add-hook 'prog-mode-hook 'evil-local-mode)
  (add-hook 'find-file-hook 'evil-local-mode))

(use-package evil-anzu
  :defer t
  :init
  (with-eval-after-load 'evil (require 'evil-anzu)))

(use-package evil-commentary
  :commands (evil-commentary-mode)
  :init
  (with-eval-after-load 'evil
    (evil-commentary-mode 1))
  :diminish evil-commentary-mode)

(use-package evil-exchange
  :commands (evil-exchange)
  :config
  (setq evil-exchange-key (kbd "gx"))
  :init
  (with-eval-after-load 'evil
    (key-seq-define evil-visual-state-map "cx" 'evil-exchange)))

(use-package evil-indent-textobject
  :defer t
  :init
  (with-eval-after-load 'evil
    (require 'evil-indent-textobject)))

(use-package evil-jumper
  :commands (global-evil-jumper-mode)
  :init
  (with-eval-after-load 'evil
    (global-evil-jumper-mode)))

(use-package evil-matchit
  :commands (global-evil-matchit-mode)
  :init
  (with-eval-after-load 'evil
    (global-evil-matchit-mode)))

(use-package evil-surround
  :commands (global-evil-surround-mode)
  :init
  (with-eval-after-load 'evil
    (global-evil-surround-mode)))

(use-package evil-visualstar
  :commands (global-evil-visualstar-mode)
  :init
  (with-eval-after-load 'evil
    (global-evil-visualstar-mode)))

;; Some Emacs bindings in evil-mode
(with-eval-after-load 'evil
  (define-minor-mode evil-rsi-mode
    "Rsi mode."
    :lighter " rsi"
    :global t
    :keymap (let ((map (make-sparse-keymap)))
              (evil-define-key 'insert map "\C-a" 'beginning-of-line)
              (evil-define-key 'motion map "\C-a" 'beginning-of-line)
              (evil-define-key 'insert map "\C-b" 'backward-char)
              (evil-define-key 'insert map "\C-d" 'delete-char)
              (evil-define-key 'insert map "\C-e" 'end-of-line)
              (evil-define-key 'motion map "\C-e" 'end-of-line)
              (evil-define-key 'insert map "\C-f" 'forward-char)
              (evil-define-key 'insert map "\C-h" 'delete-backward-char)
              map))
  (evil-rsi-mode)
  (diminish 'evil-rsi-mode))

(provide 'init-evil)
;;; init-evil.el ends here
