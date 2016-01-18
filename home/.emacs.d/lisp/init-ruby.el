;;; init-ruby.el -- ruby language specific setup

;;; Commentary:
;;; ruby setup

;;; Code:
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
(use-package splitjoin
  :commands (splitjoin)
  :config
  (bind-keys :map evil-normal-state-map
             ("gS" . 'splitjoin)
             ("gJ" . 'splitjoin)))

(defadvice ruby-indent-line (after unindent-closing-paren activate)
  "Indentation for ruby multiple line ruby methods after a opening parenthesis."
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

(add-hook 'ruby-mode-hook
          (function (lambda ()
                      (setq evil-shift-width ruby-indent-level))))

(add-hook 'ruby-mode-hook
          (lambda ()
            (modify-syntax-entry ?: "'")))

(setq flycheck-ruby-executable "/opt/rubies/ruby-2.1.5/bin/ruby")
(setq flycheck-ruby-rubocop-executable "~/rubocop.sh")
(setq ruby-insert-encoding-magic-comment nil)

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
