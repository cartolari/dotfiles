;;; init-evil.el -- Evil mode setup

;;; Commentary:
;;; Some customization, mainly for letting evil mode to be more like vim.

;;; Code:
(require 'use-package)

(use-package evil
  :bind ("C-z" . evil-local-mode)
  :config
  (setq-default evil-shift-width 2)
  (setq evil-move-beyond-eol t)

  (defun indent-current-buffer ()
    "Indent the `current-buffer'."
    (interactive)
    (indent-region (point-min) (point-max)))

  (defun evil-save-and-normal-state ()
    "Save the `current-buffer' and change to `evil-normal-state'"
    (interactive)
    (evil-normal-state)
    (save-buffer))

  (defun replace-symbol-at-point (replacement)
    "Replace `thing-at-point' with `REPLACEMENT'."
    (interactive)
    (with-symbol-and-bounds
     (when bounds
       (delete-region (car bounds) (cdr bounds))
       (insert replacement))))

  (defun transform-symbol-at-point (fn)
    "Apply `FN' to `thing-at-point' and replace it in the current buffer."
    (interactive)
    (with-symbol-and-bounds
     (replace-symbol-at-point (funcall fn text))))

  (defun snake-case-at-point ()
    "Make `thing-at-point' to be snake cased."
    (interactive)
    (transform-symbol-at-point 's-snake-case))

  (defun lower-camel-case-at-point ()
    "Make `thing-at-point' to be camel cased with the first letter lower cased."
    (interactive)
    (transform-symbol-at-point 's-lower-camel-case))

  (defun upper-camel-case-at-point ()
    "Make `thing-at-point' to be camel cased with the first letter upper cased."
    (interactive)
    (transform-symbol-at-point 's-upper-camel-case))

  (defun upper-snake-case-at-point ()
    "Make `thing-at-point' to be snake and upper cased."
    (interactive)
    (transform-symbol-at-point (lambda (str)
                                 (s-upcase (s-snake-case str)))))

  (bind-keys :map evil-normal-state-map
             (":" . evil-repeat-find-char)
             (";" . evil-ex))
  (bind-keys :map evil-visual-state-map
             (":" . evil-repeat-find-char)
             (";" . evil-ex))

  (global-set-key (kbd "C-c c s") 'snake-case-at-point)
  (global-set-key (kbd "C-c c u") 'upper-camel-case-at-point)
  (global-set-key (kbd "C-c c l") 'lower-camel-case-at-point)
  (global-set-key (kbd "C-c c c") 'upper-snake-case-at-point)

  (key-seq-define evil-insert-state-map ",s" 'evil-save-and-normal-state)
  (key-seq-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-seq-define evil-normal-state-map ",q" 'delete-window)
  (key-seq-define evil-normal-state-map ",s" 'save-buffer)
  (key-seq-define evil-normal-state-map "==" 'indent-current-buffer)
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
