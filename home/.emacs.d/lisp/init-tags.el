;;; init-tags.el -- setup tags for emacs

;;; Commentary:
;;; ggtags setup for emacs

;;; Code:
(use-package ggtags
  :commands (ggtags-mode)
  :config
  (bind-key "C-]" 'ggtags-find-tag-dwim evil-normal-state-map)
  :diminish ggtags-mode
  :init
  (add-hook 'prog-mode-hook 'ggtags-mode))

(provide 'init-tags)
;;; init-tags.el ends here
