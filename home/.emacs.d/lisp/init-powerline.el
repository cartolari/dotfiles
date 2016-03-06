;;; init-powerline.el -- Powerline configuration

;;; Commentary:
;;; Simplified mode line setup that focuses on providing only the most useful
;;; information

;;; Code:
(require 'use-package)

(defun extension-to-major-mode (filename)
  "Find which major mode will be activated when a file with FILENAME is opened."
  (cdr (cl-find filename auto-mode-alist
                :test (lambda (_ extension-mode)
                        (string-match (car extension-mode) filename)))))

(defvar major-mode-icons
  '((bat-mode . )
    (c++-mode . )
    (c-mode . )
    (clojure-mode . )
    (clojurec-mode . )
    (clojurescript-mode . )
    (coffee-mode . )
    (conf-mode . )
    (conf-mode-maybe . )
    (css-mode . )
    (d-mode . )
    (dart-mode . )
    (diff-mode . )
    (erlang-mode . )
    (fsharp-mode . )
    (gfm-mode . )
    (go-mode . )
    (haskell-mode . )
    (image-mode . )
    (java-mode . )
    (js2-mode . )
    (json-mode . )
    (julia-mode . )
    (less-css-mode . )
    (lisp-mode . λ)
    (lua-mode . )
    (perl-mode . )
    (php-mode . )
    (python-mode . )
    (ruby-mode . )
    (rus-mode . )
    (scala-mode . )
    (scss-mode . )
    (sh-mode . )
    (sql-mode . )
    (stylus-mode . )
    (typescript-mode . )
    (vimrc-mode . )
    (web-mode . )
    (yaml-mode . ))
  "List of major to its icons.")

(defvar file-extensions-to-icons
  '(("ai"       . "")
    ("bat"      . "")
    ("bmp"      . "")
    ("c"        . "")
    ("c++"      . "")
    ("cc"       . "")
    ("clj"      . "")
    ("cljc"     . "")
    ("cljs"     . "")
    ("coffee"   . "")
    ("conf"     . "")
    ("cp"       . "")
    ("cpp"      . "")
    ("css"      . "")
    ("cxx"      . "")
    ("d"        . "")
    ("dart"     . "")
    ("db"       . "")
    ("diff"     . "")
    ("dump"     . "")
    ("edn"      . "")
    ("ejs"      . "")
    ("erl"      . "")
    ("f#"       . "")
    ("fish"     . "")
    ("fs"       . "")
    ("fsi"      . "")
    ("fsscript" . "")
    ("fsx"      . "")
    ("gif"      . "")
    ("go"       . "")
    ("hbs"      . "")
    ("hrl"      . "")
    ("hs"       . "")
    ("htm"      . "")
    ("html"     . "")
    ("ico"      . "")
    ("ini"      . "")
    ("java"     . "")
    ("jl"       . "")
    ("jpeg"     . "")
    ("jpg"      . "")
    ("js"       . "")
    ("json"     . "")
    ("jsx"      . "")
    ("less"     . "")
    ("lhs"      . "")
    ("lua"      . "")
    ("markdown" . "")
    ("md"       . "")
    ("ml"       . "λ")
    ("mli"      . "λ")
    ("mustache" . "")
    ("php"      . "")
    ("pl"       . "")
    ("pm"       . "")
    ("png"      . "")
    ("psb"      . "")
    ("psd"      . "")
    ("py"       . "")
    ("pyc"      . "")
    ("pyd"      . "")
    ("pyo"      . "")
    ("rb"       . "")
    ("rlib"     . "")
    ("rs"       . "")
    ("rss"      . "")
    ("scala"    . "")
    ("scss"     . "")
    ("sh"       . "")
    ("slim"     . "")
    ("sln"      . "")
    ("sql"      . "")
    ("styl"     . "")
    ("suo"      . "")
    ("t"        . "")
    ("ts"       . "")
    ("twig"     . "")
    ("vim"      . "")
    ("xul"      . "")
    ("yml"      . ""))
  "List of filenames to icons.")

(defvar filenames-to-icons
  '((".bashprofile"                     . "")
    (".bashrc"                          . "")
    (".ds_store"                        . "")
    (".gitconfig"                       . "")
    (".gitignore"                       . "")
    ("dropbox"                          . "")
    ("exact-match-case-sensitive-1.txt" . "X1")
    ("exact-match-case-sensitive-2"     . "X2")
    ("favicon.ico"                      . "")
    ("gruntfile.coffee"                 . "")
    ("gruntfile.js"                     . "")
    ("gruntfile.ls"                     . "")
    ("gulpfile.coffee"                  . "")
    ("gulpfile.js"                      . "")
    ("gulpfile.ls"                      . "")
    ("license"                          . "")
    ("node_modules"                     . "")
    ("procfile"                         . "")
    ("react.jsx"                        . ""))
  "Exact filenames to icons.")

(defvar file-patterns-to-icons
  '((".*jquery.*\.js$"       . "")
    (".*angular.*\.js$"      . "")
    (".*backbone.*\.js$"     . "")
    (".*require.*\.js$"      . "")
    (".*materialize.*\.js$"  . "")
    (".*materialize.*\.css$" . "")
    (".*mootools.*\.js$"     . ""))
  "File name patterns to icons.")

(defvar file-encodings-to-icons
  '(("dos" . "")
    ("mac" . "")
    ("unix" . ""))
  "File encodings to icons.")

(defun major-mode-to-icon  (major-mode)
  "Icon for MAJOR-MODE."
  (cdr (assoc major-mode major-mode-icons)))

;; Remember to append utf-8, utf-7, undecided or whatever to the mode line
(defun file-to-encoding-icon (encoding)
  "Icon for ENCODING."
  (let* ((buf-coding (format "%s" buffer-file-coding-system))
         (icon (cdr (cl-find
                     buf-coding file-encodings-to-icons
                     :test (lambda (_ encoding)
                             (string-match (car encoding) buf-coding))))))
    (or icon buf-coding)))

(defun file-name-exact-icon (filename)
  "Icon for the exact FILENAME, like Procfile for example."
  (cdr (assoc (downcase filename) filenames-to-icons)))

(defun file-name-extension-icon (filename)
  "Icon for FILENAME extension."
  (cdr (assoc (file-name-extension (downcase filename)) file-extensions-to-icons)))

(defun file-name-pattern-icon (filename)
  "Special icon for library icons for FILENAME if it match a pattern.
For more info check the `file-patterns-to-icons' variable."
  (let ((found-pattern-icon (cl-find nil file-patterns-to-icons :test
                                     (lambda (_ pattern-icon)
                                       (string-match (car pattern-icon) filename)))))
    (cdr found-pattern-icon)))

(defun file-name-to-icon (filename)
  "Icon for FILENAME extension if one exist."
  (if filename
      (or (file-name-exact-icon filename)
          (file-name-pattern-icon filename)
          (file-name-extension-icon filename))))

(defun change-mode-name-to-icon ()
  "Change the current `mode-name' to an icon.
Leaves `mode-name' unchanged if no icon is found."
  (setq mode-name (or (file-name-to-icon (buffer-file-name))
                      (major-mode-to-icon major-mode)
                      mode-name)))

(add-hook 'after-change-major-mode-hook 'change-mode-name-to-icon)

(use-package eyebrowse
  :commands (eyebrowse-mode)
  :config
  (setq eyebrowse-new-workspace t)
  (bind-key "s-w" (defhydra hydra-eyebrowse ()
                    "eyebrowse"
                    ("n" eyebrowse-next-window-config "next")
                    ("N" (lambda ()
                           (interactive)
                           (eyebrowse-switch-to-window-config
                            (+ 1 (eyebrowse--get 'current-slot))))
                     "new")
                    ("p" eyebrowse-prev-window-config "previous")
                    ("c" eyebrowse-close-window-config "close")
                    ("l" eyebrowse-last-window-config "last")
                    ("q" nil "quit"))
            eyebrowse-mode-map)
  :init
  (add-hook 'after-init-hook 'eyebrowse-mode))

(use-package window-numbering
  :commands (window-numbering-mode)
  :config
  (defun window-numbering-install-mode-line (&optional ignored))
  :init
  (add-hook 'after-init-hook 'window-numbering-mode))

(use-package spaceline
  :init
  (add-hook
   'after-init-hook
   (lambda ()
     (spaceline-define-segment buffer-encoding-abbrev-icon
       "The line ending convention used in the buffer."
       (file-to-encoding-icon buffer-file-coding-system))
     (require 'spaceline-segments)
     ;; change setq to custom-set-variables
     (setq anzu-cons-mode-line-p nil)
     (setq anzu-mode-line-update-function
           (lambda (here total)
             (propertize (format "<%d/%d>" here total)
                         'face (spaceline-highlight-face-default))))
     (setq powerline-default-separator 'wave)
     (setq powerline-height 25)
     (setq spaceline-window-numbers-unicode t)
     (setq spaceline-workspace-numbers-unicode t)
     (set-face-attribute 'mode-line nil :font "Source Code Pro for Powerline-10")
     (set-face-attribute (spaceline-highlight-face-default) nil :weight 'bold)
     (spaceline-install
      '(((anzu workspace-number window-number evil-state) :face highlight-face :separator " | ")
        (buffer-modified buffer-size buffer-id remote-host)
        ;; (major-mode :face mode-by-extension)
        ;; (major-mode :face highlight-face)
        major-mode
        ((flycheck-error flycheck-warning flycheck-info) :when active)
        (((minor-modes :separator " ") process) :when active)
        (version-control :when active))
      `(selection-info
        ;; ((buffer-encoding-abbrev point-position line-column) :separator " | ")
        ((buffer-encoding-abbrev-icon point-position line-column) :separator " | ")
        (global :when active)
        buffer-position
        hud))
     (setq mode-line-format (default-value 'mode-line-format)))))

(defadvice vc-mode-line (after strip-backend () activate)
  "Remove the Git string from the 'vc-mode-line'."
  (when (stringp vc-mode)
    (let ((noback (replace-regexp-in-string
                   (format "^ %s\\(:\\|-\\)?" (vc-backend buffer-file-name))
                   " " vc-mode)))
      (setq vc-mode noback))))

(provide 'init-powerline)
;;; init-powerline.el ends here
