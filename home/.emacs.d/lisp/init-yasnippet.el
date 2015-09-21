;;; init-yasnippet.el -- yasnippet setup

;;; Commentary:
;;; yasnippet and custom snippets

;;; Code:
(require-package 'yasnippet)

(require 'yasnippet)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets" "~/code/yasnippet-snippets"))

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or
       (not company-mode/enable-yas)
       (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends
      (mapcar #'company-mode/backend-with-yas company-backends))

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
  (local-set-key (kbd "<tab>") 'tab-indent-or-complete))

(add-hook 'prog-mode-hook 'map-company-yasnippet-tab)

(define-key company-active-map [tab] 'company-select-next)
(define-key company-active-map [backtab] 'company-select-previous)
(define-key company-active-map (kbd "<tab>") 'company-select-next)
(define-key company-active-map (kbd "<backtab>") 'company-select-previous)

(yas-global-mode 1)

(diminish 'yas-minor-mode)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
