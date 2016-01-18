;;; init-completion.el -- Code completion setup

;;; Commentary:
;;; completion setup

;;; Code:
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
  (add-hook-for-modes
   (prog-mode-hook yaml-mode-hook)
   (add-to-list 'completion-at-point-functions 'my/dabbrev-capf t))
  (add-to-list 'company-transformers 'remove-thing-at-point-transform)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 0
        dabbrev-abbrev-skip-leading-regexp ":")
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

(use-package company-flx
  :commands (company-flx-mode)
  :init
  (add-hook 'global-company-mode-hook 'company-flx-mode))

(global-set-key (kbd "M-/") 'hippie-expand)

(defun company-mode/backend-with-yas (backend)
  "Joins a company-mode BACKEND with the YASnippet company-yasnippet backend."
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  "Expand a YAS snippet."
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(defun map-company-yasnippet-tab ()
  "Map tab to company, yasnippet or indent."
  (bind-keys :map global-map
             ((local-set-key [tab] 'tab-indent-or-complete)
              (local-set-key (kbd "TAB") 'tab-indent-or-complete))))

(defun my/dabbrev-find-all (abbrev)
  "Return a list of expansions matched by ABBREV."
  (let ((dabbrev-check-other-buffers t)
        (dabbrev-check-all-buffers nil))
    (flet ((message (&rest args)))
      (dabbrev--reset-global-variables)
      (dabbrev--find-all-expansions abbrev t))))

(defun my/dabbrev-capf ()
  "Dabbrev 'complete-at-point-functions' implementation."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (list
     (car bounds)
     (cdr bounds)
     (my/dabbrev-find-all (substring (thing-at-point 'symbol t) 0 1))
     :exclusive 'no)))

(defun my/yasnippet-candidate-p (candidate)
  "Check if a company mode CANDIDATE came from company-yasnippet backend."
  (not (null (get-text-property 0 'yas-template candidate))))

(defun remove-thing-at-point-transform (candidates)
  "Remove 'thing-at-point' from CANDIDATES."
  (let ((current-symbol (thing-at-point 'symbol)))
    (cl-remove current-symbol candidates
               :test (lambda (ignore candidate)
                       (and (not (my/yasnippet-candidate-p candidate))
                            (string= candidate current-symbol))))))

(provide 'init-completion)
;;; init-completion.el ends here