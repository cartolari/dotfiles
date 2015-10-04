;;; init-elpa.el -- setup emacs package management

;;; Commentary:
;;; load package.el and provide functions to install packages from there

;;; Code:
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be re-downloaded
in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(require 'package)

(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(setq package-enable-at-startup nil)

(provide 'init-elpa)
;;; init-elpa.el ends here
