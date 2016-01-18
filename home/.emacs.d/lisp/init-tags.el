;;; init-tags.el -- setup tags for emacs

;;; Commentary:
;;; ggtags setup for emacs

;;; Code:
(require 'use-package)

(use-package ggtags
  :commands (ggtags-mode)
  :config
  (with-eval-after-load 'evil
    (bind-key "C-]" 'ggtags-find-tag-dwim evil-normal-state-map))
  (setq ggtags-mode-line-project-name nil)
  :diminish ggtags-mode
  :init
  (add-hook 'prog-mode-hook 'ggtags-mode))

(provide 'init-tags)
;;; init-tags.el ends here
