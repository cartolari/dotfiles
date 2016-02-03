;;; init-completion.el -- Code completion setup

;;; Commentary:
;;; completion setup

;;; Code:
(require 'use-package)

(use-package auto-complete
  :config
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  (setq ac-auto-show-menu t
        ac-auto-start t
        ac-candidate-limit 100
        ac-delay 0
        ac-ignore-case t
        ac-quick-help-delay 0
        ac-use-menu-map t)
  (ac-flyspell-workaround)
  (ac-linum-workaround)
  (bind-keys :map ac-menu-map
             ("TAB" . ac-next)
             ([tab] . ac-next)
             ("<backtab>" . ac-previous)
             ([backtab] . ac-previous)
             ("C-n" . ac-next)
             ("C-p" . ac-previous))
  (defun sanityinc/fci-enabled-p () (symbol-value 'fci-mode))
  (defvar sanityinc/fci-mode-suppressed nil)
  (make-variable-buffer-local 'sanityinc/fci-mode-suppressed)
  (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (let ((fci-enabled (sanityinc/fci-enabled-p)))
      (when fci-enabled
        (setq sanityinc/fci-mode-suppressed fci-enabled)
        (turn-off-fci-mode))))
  (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and sanityinc/fci-mode-suppressed
               (null popup-instances))
      (setq sanityinc/fci-mode-suppressed nil)
      (turn-on-fci-mode)))

  (set-default 'ac-sources
               '(ac-source-imenu
                 ac-source-dictionary
                 ac-source-words-in-buffer
                 ac-source-words-in-same-mode-buffers
                 ac-source-words-in-all-buffer
                 ac-source-yasnippet
                 ac-source-gtags
                 ac-source-filename))
  (add-to-list 'completion-styles 'initials t)
  :init
  (add-hook 'after-init-hook 'global-auto-complete-mode))

(use-package ac-capf
  :commands (ac-capf-setup)
  :init
  (add-hook 'prog-mode-hook 'ac-capf-setup)
  (add-hook 'eshell-mode-hook 'ac-capf-setup))

;; (global-set-key (kbd "M-/") 'hippie-expand)

(provide 'init-completion)
;;; init-completion.el ends here
