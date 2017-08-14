;;; init-ruby.el -- ruby language specific setup

;;; Commentary:
;;; ruby setup

;;; Code:
(require 'use-package)

(use-package inf-ruby
  :commands (inf-ruby inf-ruby-mode))

(use-package projectile-rails
  :commands (projectile-rails-on)
  :config
  (define-key projectile-rails-mode-map (kbd "s-r") 'hydra-projectile-rails/body)
  :diminish projectile-rails-mode
  :init
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package robe
  :commands (robe-mode))

(use-package rspec-mode
  :commands (rspec-mode))

(use-package ruby-end
  :defer t
  :diminish ruby-end-mode
  :init
  (add-hook 'ruby-mode-hook #'ruby-end-mode))

(use-package ruby-hash-syntax
  :commands (ruby-toggle-hash-syntax))

(use-package ruby-refactor
  :commands (ruby-refactor-mode)
  :diminish ruby-refactor-mode
  :init
  (add-hook 'ruby-mode-hook #'ruby-refactor-mode))

(use-package slim-mode
  :mode "\\.slim\\'")

(use-package splitjoin
  :commands (splitjoin)
  :config
  (bind-keys :map evil-normal-state-map
             ("gS" . 'splitjoin)
             ("gJ" . 'splitjoin)))

(use-package ruby-mode
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'insert ruby-mode-map "#" 'ruby-tools-interpolate))
  (setq ruby-align-chained-calls nil
        ruby-align-to-stmt-keywords nil
        ruby-deep-indent-paren nil
        ruby-deep-indent-paren-style nil
        ruby-use-smie nil
        ruby-insert-encoding-magic-comment nil)

  (add-hook 'ruby-mode-hook (lambda () (modify-syntax-entry ?: "'")))

  (defun ruby-tools-looking-around (back at)
    "Check if looking backwards at BACK and forward at AT."
    (and (looking-at-p at) (looking-back back)))
  (defun ruby-tools-interpolate ()
    "Interpolate with #{} in some places."
    (interactive)
    (if (and mark-active (equal (point) (region-end)))
        (exchange-point-and-mark))
    (insert "#")
    (when (or (ruby-tools-looking-around "\"[^\"\n]*" "[^\"\n]*\"")
              (ruby-tools-looking-around "`[^`\n]*"   "[^`\n]*`")
              (ruby-tools-looking-around "%([^(\n]*"  "[^)\n]*)"))
      (cond (mark-active (goto-char (region-beginning))
                         (insert "{")
                         (goto-char (region-end))
                         (insert "}"))
            (t (insert "{}")
               (forward-char -1)))))

  (defadvice ruby-indent-line (after unindent-closing-paren activate)
    "Indentation for ruby multiple line methods after a opening parenthesis."
    (let ((column (current-column))
          indent offset)
      (save-excursion
        (back-to-indentation)
        (let ((state (syntax-ppss)))
          (setq offset (- column (current-column)))
          (when (and (eq (char-after) ?\))
                     (not (zerop (car state))))
            (goto-char (cadr state))
            (setq indent (current-indentation)))))
      (when indent
        (indent-line-to indent)
        (when (> offset 0) (forward-char offset)))))
  :ensure nil)

(defun rspec-compile-file ()
  (interactive)
  (compile (format "cd %s; docker-compose run --rm web bundle exec rspec %s"
                   (projectile-project-root)
                   (file-relative-name (buffer-file-name) (projectile-project-root))
                   ) t))

(defun rspec-compile-on-line ()
  (interactive)
  (compile (format "cd %s; docker-compose run --rm web bundle exec rspec %s -l %s"
                   (projectile-project-root)
                   (file-relative-name (buffer-file-name) (projectile-project-root))
                   (line-number-at-pos)
                   ) t))

(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c r f") 'rspec-compile-file)
            (local-set-key (kbd "C-c r n") 'rspec-compile-on-line)))

(provide 'init-ruby)
;;; init-ruby.el ends here
