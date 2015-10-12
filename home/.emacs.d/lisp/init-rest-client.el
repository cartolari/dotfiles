;;; init-rest-client.el -- Emacs rest client

;;; Commentary:
;;; Rest client emacs plugin

;;; Code:
(use-package restclient
  :config
  (add-to-list 'company-backends 'company-restclient)
  (use-package company-restclient)
  :mode ("\\.http\\'" . restclient-mode))

(provide 'init-rest-client)
;;; init-rest-client.el ends here
