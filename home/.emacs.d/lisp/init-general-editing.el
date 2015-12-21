;;; init-general-editing.el --- General bindings

;;; Commentary:
;;; Bindings that are useful in multiple or all modes

;;; Code:
(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package discover-my-major
  :commands (discover-my-major discover-my-mode))

(use-package exec-path-from-shell
  :defer t
  :init
  (add-hook 'after-init-hook 'exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package guide-key
  :commands (guide-key-mode)
  :config
  (setq echo-keystrokes 0.3)
  (setq guide-key/guide-key-sequence t)
  (setq guide-key/idle-delay 0.3)
  :init
  (add-hook 'after-init-hook 'guide-key-mode)
  :diminish guide-key-mode)

(use-package hideshow
  :commands (hs-minor-mode)
  :bind ("<C-return>" . hs-toggle-hiding)
  :diminish hs-minor-mode
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode))

(use-package highlight-escape-sequences
  :commands (hes-mode)
  :config
  (put 'hes-escape-backslash-face 'face-alias 'font-lock-builtin-face)
  (put 'hes-escape-sequence-face 'face-alias 'font-lock-builtin-face)
  :init
  (add-hook 'after-init-hook 'hes-mode))

(use-package keyfreq
  :commands (keyfreq-mode)
  :init
  (add-hook 'after-init-hook 'keyfreq-mode))

(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :init
  (autoload 'rainbow-delimiters-mode "rainbow-delimiters" nil t)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package undo-tree
  :commands (global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  :diminish undo-tree-mode
  :init
  (add-hook 'after-init-hook 'global-undo-tree-mode))

(use-package whitespace-cleanup-mode
  :commands (whitespace-cleanup-mode)
  :diminish (whitespace-cleanup-mode . "Ⓦ")
  :init
  (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun sudo-save ()
  "Save a file using sudo."
  (interactive)
  (let ((file-name (or buffer-file-name
                       (ido-read-file-name "File:"))))
    (write-file (concat "/sudo:root@localhost:" file-name))))

(defun display-ansi-colors ()
  "Display ANSI escape sequences in a buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer)
                 (not (buffer-modified-p buffer)))
        (set-buffer buffer)
        (revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshed open files"))

(add-hook
 'after-init-hook
 (lambda ()
   (progn
     (column-number-mode)
     (delete-selection-mode 1)
     (electric-pair-mode 1)
     (global-auto-revert-mode)
     (global-linum-mode 1)
     (keyfreq-autosave-mode 1)
     (savehist-mode 1)
     (show-paren-mode 1))))

(set-default 'truncate-lines t)
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq create-lockfiles nil)
(setq disabled-command-function nil)
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring))
(setq truncate-partial-width-windows nil)
(setq-default display-buffer-reuse-frames t)
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)

(use-package whitespace
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tab-mark trailing))
  (add-hook 'prog-mode-hook 'whitespace-mode)
  :diminish whitespace-mode
  :ensure nil)

(setq-default tab-width 2)

(provide 'init-general-editing)
;;; init-general-editing.el ends here
