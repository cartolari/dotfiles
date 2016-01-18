;;; init-byte-compilation.el -- Automatic byte compile Emacs Lisp

;;; Commentary:
;;; Provides automatic byte compilation on save and on load for Emacs Lisp files

;;; Code:
(require 'use-package)

(require-package 'auto-compile)

(require 'auto-compile)

(setq load-prefer-newer t)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

(provide 'init-byte-compilation)
;;; init-byte-compilation.el ends here
