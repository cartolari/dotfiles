;;; init-vim.el -- Support for editing vimrc files in Emacs

;;; Commentary:
;;; Vimrc syntax highlighting

;;; Code:
(require 'use-package)

(use-package vimrc-mode
  :diminish ""
  :mode (".vim\\(rc\\)?$" . vimrc-mode))

(provide 'init-vim)
;;; init-vim.el ends here
