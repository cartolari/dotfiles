;;; init-multiple-cursors.el -- multiple cursors setup

;;; Commentary:
;;; custom multiple cursors setup, mainly to work with evil mode

;;; Code:
(use-package multiple-cursors
  :config
  (require 'mc-cycle-cursors)
  (use-package phi-search)
  (add-hook 'multiple-cursors-mode-enabled-hook
            'evil-multiple-cursors/switch-to-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook
            'evil-multiple-cursors/back-to-previous-state))
(use-package region-bindings-mode
  :config
  (region-bindings-mode-enable)
  (bind-key "C->" 'mc/mark-next-like-this region-bindings-mode-map)
  (bind-key "C-<" 'mc/mark-previous-like-this region-bindings-mode-map)
  (bind-key "C-c C->" 'mc/skip-to-next-like-this region-bindings-mode-map)
  :diminish region-bindings-mode)

;; =============================================================
;; Multiple cursors evil compat (use emacs mode during mc)
;; =============================================================
(defvar evil-multiple-cursors/evil-prev-state nil)

(defun evil-multiple-cursors/switch-to-emacs-state ()
  "If in evil-mode return to emacs-state so multiple-cursors works."
  (when (evil-local-mode)

    (setq evil-multiple-cursors/evil-prev-state t)

    (let ((mark-before (mark))
          (point-before (point)))

      (evil-emacs-state 1)

      (goto-char point-before)
      (set-mark mark-before))))

(defun evil-multiple-cursors/back-to-previous-state ()
  "After editing in multiple-cursors return to the previous evil-mode state."
  (when evil-multiple-cursors/evil-prev-state
    (evil-normal-state)
    (setq evil-multiple-cursors/evil-prev-state nil)))

(provide 'init-multiple-cursors)
;;; init-multiple-cursors.el ends here
