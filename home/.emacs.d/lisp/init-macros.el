;;; init-macros.el -- My custom Emacs macros

;;; Commentary:
;;; Macros that will be available to other files in the Lisp folder

;;; Code:
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defmacro add-hook-for-modes (modes hook &optional arg)
  "For each mode in MODES add HOOK to each.
Optionally generates a lambda with an arg called arg if ARG is t"
  `(dolist (mode '(,@modes))
     (add-hook mode (lambda ,(if arg '(arg)) (,@hook)))))

(defmacro with-symbol-and-bounds (body)
  "Put bounds and text variables into BODY context.
`bounds' is `bounds-of-thing-at-point'.
`text' is the substring delimited by `bounds-of-thing-at-point'"
  `(let* ((bounds (bounds-of-thing-at-point 'symbol))
          (text   (buffer-substring-no-properties (car bounds) (cdr bounds))))
     ,body))

(provide 'init-macros)
;;; init-macros.el ends here
