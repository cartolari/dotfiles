;;; init-ansible.el -- Ansible specificy setup

;;; Commentary:
;;; Minor mode for Ansible.  Load snippets and add vault tools

;;; Code:
(use-package ansible
  :commands (ansible)
  :diminish ansible
  :init
  (with-eval-after-load 'ansible 'ansible/fix-snippets))

(defun ansible/fix-snippets ()
  "Fix ansible snippets triggering in wrong modes."
  (let* ((pkg-dir (package-desc-dir (cadr (assq 'ansible package-alist))))
         (txt-dir (concat pkg-dir "/snippets/text-mode/"))
         (yml-dir (concat pkg-dir "/snippets/yaml-mode/")))
    (when (file-directory-p txt-dir)
      (when (file-directory-p yml-dir)
        (delete-directory yml-dir t))
      (rename-file txt-dir yml-dir))))

(provide 'init-ansible)
;;; init-ansible.el ends here
