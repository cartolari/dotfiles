;;; init-ruby.el -- ruby language specific setup

;;; Commentary:
;;; ruby setup

;;; Code:
(require-package 'chruby)
(require-package 'inf-ruby)
(require-package 'projectile-rails)
(require-package 'robe)
(require-package 'rspec-mode)
(require-package 'ruby-end)
(require-package 'ruby-hash-syntax)
(require-package 'ruby-refactor)
(require-package 'splitjoin)

(require 'chruby)
(require 'compile)
(require 'robe)
(require 'ruby-end)
(require 'ruby-refactor)

(chruby-use "2.1.5")

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
            (local-set-key (kbd "C-c r n") 'rspec-compile-on-line)
            ))

(add-hook 'ruby-mode-hook 'hs-minor-mode)
(add-to-list 'hs-special-modes-alist
             `(ruby-mode
               ,(rx (or "[" "{"
                        "class" "module" "def" "do"
                        "begin" "ensure" "rescue"
                        "case" "if" "unless")) ; Block start
               ,(rx (or "}" "]" "end"))        ; Block end
               ,(rx (or "#" "=begin"))         ; Comment start
               ruby-forward-sexp nil))
(add-hook 'projectile-mode-hook 'projectile-rails-on)

(add-hook 'ruby-mode-hook (lambda () (ruby-refactor-mode 1)))

(define-key evil-normal-state-map (kbd "gS") 'splitjoin)
(define-key evil-normal-state-map (kbd "gJ") 'splitjoin)

(diminish 'ruby-end-mode)
(diminish 'ruby-refactor-mode)

(provide 'init-ruby)
;;; init-ruby.el ends here
