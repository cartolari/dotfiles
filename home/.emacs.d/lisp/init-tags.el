;;; init-tags.el -- setup tags for emacs

;;; Commentary:
;;; ggtags setup for emacs

;;; Code:
(require-package 'ggtags)
(require-package 'helm-gtags)

(add-hook 'prog-mode-hook 'ggtags-mode)
(define-key evil-normal-state-map (kbd "C-]") 'ggtags-find-tag-dwim)

(provide 'init-tags)
;;; init-tags.el ends here
