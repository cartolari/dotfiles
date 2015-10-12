;;; init-powerline.el -- Powerline configuration

;;; Commentary:
;;; Try to mimic Spacemacs modeline

;;; Code:
(use-package powerline)

(defface powerline-custom1 '((t (:background "#EEAD0E" :foreground "black" :weight bold)))
         "Custom face for bright sections"
         :group 'powerline)

(defface powerline-custom2 '((t (:foreground "#EEAD0E" :weight bold)))
  "Custom face for text"
  :group 'powerline)

(defface powerline-flycheck-error '((t (:foreground "#F07C6D" :weight bold)))
  "Face for flycheck errors in modeline"
  :group 'powerline)

(defface powerline-flycheck-warning '((t (:foreground "#D9B24F" :weight bold)))
         "Face for flycheck errors in modeline"
         :group 'powerline)

(defface powerline-flycheck-info '((t (:foreground "#76B7ED" :weight bold)))
         "Face for flycheck errors in modeline"
         :group 'powerline)

(defun powerline-flycheck-errors (error-type)
  "Number of ERROR-TYPE errors for the current Flycheck buffer."
  (let ((item-count (cdr (assoc error-type (flycheck-count-errors
                                            flycheck-current-errors)))))
    (format " • %d" (or item-count 0))))

(defun powerline-evil-state ()
  "Current evil-mode state if any."
  (if 'evil-local-mode
      (cond ((evil-insert-state-p) "<I>")
            ((evil-visual-state-p) "<V>")
            ((evil-replace-state-p) "<R>")
            ((evil-emacs-state-p) "<E>")
            ((evil-motion-state-p) "<M>")
            ((evil-operator-state-p) "<O>")
            ((evil-normal-state-p) "<N>"))))

;;; TODO: remove require anzu from here
(require 'anzu)
(defun my/anzu-update-func (here total)
  "Customizing how anzu displays HERE & TOTAL on the mode line."
  (propertize (format " <%d/%d>" here total)
              'face 'powerline-custom1))
(setq anzu-mode-line-update-function 'my/anzu-update-func)

(defun powerline-spacemacs-imitation-theme ()
  "An attempt to imitate the spacemacs powerline theme."
  (interactive)
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
             (mode-line (if active 'mode-line 'mode-line-inactive))
             (face1 (if active 'powerline-active1 'powerline-inactive1))
             (face2 (if active 'powerline-active2 'powerline-inactive2))
             (face3 (if active 'powerline-custom1 mode-line))
             (face4 (if active 'powerline-custom2 mode-line))
             (separator-left (intern (format "powerline-%s-%s"
                                             (powerline-current-separator)
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              (powerline-current-separator)
                                              (cdr powerline-default-separator-dir))))
             (lhs (list (powerline-raw "%*" face3 'l)
                        (powerline-raw " " face3)
                        (powerline-raw (powerline-evil-state) face3)
                        (funcall separator-left face3 mode-line)

                        (when powerline-display-buffer-size
                          (powerline-buffer-size nil 'l))
                        (when powerline-display-mule-info
                          (powerline-raw mode-line-mule-info face4 'l))
                        (powerline-buffer-id face4 'l)
                        (when (and (boundp 'which-func-mode) which-func-mode)
                          (powerline-raw which-func-format nil 'l))
                        (powerline-raw " ")
                        (funcall separator-right mode-line face1)

                        (when (boundp 'erc-modified-channels-object)
                          (powerline-raw erc-modified-channels-object face1 'l))
                        (powerline-major-mode face1 'l)
                        (powerline-process face1)
                        (powerline-raw " " face1)
                        (funcall separator-left face1 mode-line)

                        ;; FLYCHECK start
                        (powerline-raw (powerline-flycheck-errors 'error)
                                       'powerline-flycheck-error)
                        (powerline-raw (powerline-flycheck-errors 'warning)
                                       'powerline-flycheck-warning)
                        (powerline-raw (powerline-flycheck-errors 'info)
                                       'powerline-flycheck-info)

                        (powerline-raw " " mode-line)

                        (funcall separator-right mode-line face1)
                        ;; FLYCHECK end

                        (powerline-minor-modes face1 'l)
                        (powerline-raw " " face1)
                        (funcall separator-left face1 mode-line)

                        (powerline-vc mode-line 'r)
                        (powerline-raw " " mode-line)
                        (funcall separator-right mode-line face2)))
             (rhs (list (powerline-raw global-mode-string face2 'r)
                        (funcall separator-right face2 face1)
                        (powerline-raw "%4l" face1 'l)
                        (powerline-raw ":" face1 'l)
                        (powerline-raw "%3c" face1 'r)
                        (funcall separator-right face1 mode-line)
                        (powerline-raw " ")
                        (powerline-raw "%6p" nil 'r))))
        (concat (powerline-render lhs)
                (powerline-fill face2 (powerline-width rhs))
                (powerline-render rhs)))))))

(defun set-mode-line-font ()
  "Update mode-line font.
Needs investigation on why it's needed to setup as an
hook otherwise the settings didn't get applied"
  (set-face-attribute 'mode-line nil :font "Source Code Pro for Powerline-10"))

(defadvice vc-mode-line (after strip-backend () activate)
  "Remove the Git string from the 'vc-mode-line'."
  (when (stringp vc-mode)
    (let ((noback (replace-regexp-in-string
                   (format "^ %s" (vc-backend buffer-file-name))
                   " " vc-mode)))
      (setq vc-mode noback))))

(powerline-spacemacs-imitation-theme)
(setq powerline-default-separator 'wave)
(setq powerline-default-separator-dir '(right . left))
(setq powerline-height 25)

(add-hook 'after-init-hook 'powerline-reset)
(add-hook 'after-init-hook 'set-mode-line-font)
(provide 'init-powerline)
;;; init-powerline.el ends here
