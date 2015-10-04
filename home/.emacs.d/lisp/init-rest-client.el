;;; init-rest-client.el -- Emacs rest client

;;; Commentary:
;;; Rest client emacs plugin

;;; Code:
(require-package 'restclient)
(require-package 'company-restclient)

(add-to-list 'company-backends 'company-restclient)

(provide 'init-rest-client)
;;; init-rest-client.el ends here
