;;; init-completion.el -- Code completion setup

;;; Commentary:
;;; completion setup

;;; Code:
(require 'use-package)

(defun company-mode/backend-with-yas (backend)
  "Joins a company-mode BACKEND with the YASnippet company-yasnippet backend."
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(use-package company
  :commands (global-company-mode)
  :config
  (require 'dabbrev)
  (bind-keys :map company-active-map
             ("TAB" . company-select-next)
             ([tab] . company-select-next)
             ([backtab] . company-select-previous)
             ("<backtab>" . company-select-previous)
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous))
  (add-hook-for-modes
   (company-completion-cancelled-hook company-completion-finished-hook)
   (fci-mode 1)
   t)
  (add-hook 'company-completion-started-hook (lambda (arg) (fci-mode 0)))
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-idle-delay 0.1
        company-minimum-prefix-length 0
        company-show-numbers t
        dabbrev-abbrev-skip-leading-regexp ":")
  (add-to-list 'company-backends 'company-ispell t)
  (setq company-backends
        (mapcar 'company-mode/backend-with-yas company-backends))
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-quickhelp
  :commands (company-quickhelp-mode)
  :init
  (add-hook 'global-company-mode-hook 'company-quickhelp-mode))

(use-package company-try-hard
  :commands (company-try-hard)
  :init
  (global-set-key (kbd "C-'") 'company-try-hard))

(use-package company-ycmd
  :commands (global-ycmd-mode)
  :diminish ycmd-mode
  :init
  (setq ycmd-default-tags-file-name ".git/tags"
        ycmd-server-command '("python3" "/home/bruno/.vim/bundle/youcompleteme/third_party/ycmd/ycmd")
        ycmd-tag-files 'auto)
  (add-hook-for-modes
   (prog-mode-hook yaml-mode-hook)
   (unless (string-match "lisp" (symbol-name major-mode))
     (make-local-variable 'company-backends)
     (add-to-list 'company-backends
                  (company-mode/backend-with-yas 'company-ycmd))))
  (add-hook 'after-init-hook 'global-ycmd-mode))

(use-package flycheck-ycmd
  :config
  (flycheck-ycmd-setup)
  ;; Make sure the flycheck cache sees the parse results
  (add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results)
  ;; Add the ycmd checker to the list of available checkers
  (add-to-list 'flycheck-checkers 'ycmd))

(use-package company-flx
  :commands (company-flx-mode)
  :config
  (setq company-flx-limit 50)
  :init
  (add-hook 'global-company-mode-hook 'company-flx-mode))

(global-set-key (kbd "M-/") 'hippie-expand)

(provide 'init-completion)
;;; init-completion.el ends here
