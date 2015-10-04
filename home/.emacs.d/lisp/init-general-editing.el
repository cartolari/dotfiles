;;; init-general-editing.el --- General bindings

;;; Commentary:
;;; Bindings that are useful in multiple or all modes

;;; Code:

(require-package 'ace-window)
(require-package 'aggressive-indent)
(require-package 'exec-path-from-shell)
(require-package 'expand-region)
(require-package 'framemove)
(require-package 'guide-key)
(require-package 'highlight-escape-sequences)
(require-package 'keyfreq)
(require-package 'linum-relative)
(require-package 'rainbow-delimiters)
(require-package 'undo-tree)
(require-package 'whitespace-cleanup-mode)

(require 'aggressive-indent)
(require 'framemove)
(require 'guide-key)
(require 'hideshow)
(require 'highlight-escape-sequences)
(require 'keyfreq)
(require 'linum-relative)
(require 'rainbow-delimiters)
(require 'undo-tree)
(require 'whitespace-cleanup-mode)

(exec-path-from-shell-initialize)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

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

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

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

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(global-set-key (kbd "<C-return>") 'hs-toggle-hiding)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-o") 'ace-window)

(with-eval-after-load 'linum
  (set-face-background 'linum nil)

  (require 'linum-relative)

  ;; truncate current line to four digits
  (defun linum-relative (line-number)
    (let* ((diff1 (abs (- line-number linum-relative-last-pos)))
           (diff (if (minusp diff1)
                     diff1
                   (+ diff1 linum-relative-plusp-offset)))
           (current-p (= diff linum-relative-plusp-offset))
           (current-symbol (if (and linum-relative-current-symbol current-p)
                               (if (string= "" linum-relative-current-symbol)
                                   (number-to-string (% line-number 1000))
                                 linum-relative-current-symbol)
                             (number-to-string diff)))
           (face (if current-p 'linum-relative-current-face 'linum)))
      (propertize (format linum-relative-format current-symbol) 'face face)))


  (setq
   linum-relative-current-symbol ""
   linum-relative-format "%3s "
   linum-delay t)

  (set-face-attribute 'linum-relative-current-face nil
                      :weight 'extra-bold
                      :foreground nil
                      :background nil
                      :inherit '(hl-line default)))

(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'whitespace-cleanup-mode)

(column-number-mode)
(delete-selection-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode)
(global-linum-mode 1)
(guide-key-mode 1)
(hes-mode 1)
(key-chord-mode 1)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(linum-relative-on)
(savehist-mode 1)
(show-paren-mode 1)

;; Highlight escape sequences
(put 'hes-escape-backslash-face 'face-alias 'font-lock-builtin-face)
(put 'hes-escape-sequence-face 'face-alias 'font-lock-builtin-face)

(set-default 'truncate-lines t)
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq create-lockfiles nil)
(setq disabled-command-function nil) ;; Enabled disabled commands
(setq font-lock-maximum-decoration 3)
(setq guide-key/guide-key-sequence t)
(setq key-chord-two-keys-delay 0.5)
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring))
(setq truncate-partial-width-windows nil)
(setq-default display-buffer-reuse-frames t)
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)

(add-hook 'shell-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'term-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(setq-default tab-width 2)

(diminish 'hs-minor-mode " ☰")
(diminish 'undo-tree-mode)
(diminish 'guide-key-mode)
(diminish 'whitespace-cleanup-mode " Ⓦ")

(provide 'init-general-editing)
;;; init-general-editing.el ends here
