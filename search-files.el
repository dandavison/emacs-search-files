;;; search-files.el --- search files using `git grep` or other backends
;; Version: 0.0.1
;; Author: dandavison7@gmail.com
;; Package-Requires: ((projectile "0.10.0"))
;; Keywords: search, git, projectile
;; URL: https://github.com/dandavison/emacs-search-files

(require 'cl)
(require 'projectile)
(require 'filter-results)  ;; https://github.com/dandavison/emacs-filter-results

(defvar search-files-ag-arguments '("-z"))

(defvar search-files-results-buffer-name "*search*")

;;;###autoload
(defun search-files-read-from-minibuffer (&optional search-for-definition-p)
  "Search for word entered in minibuffer.

  With prefix arg search for definition (prefix match).
  With two prefix args definition match must be exact"
  (interactive "P")
  (search-files-for-string-or-definition
   (read-from-minibuffer "Regexp: ")
   search-for-definition-p))

;;;###autoload
(defun search-files-thing-at-point (&optional search-for-definition-p)
  "Search for word at point.

  With prefix arg search for definition (prefix match).
  With two prefix args definition match must be exact"
  (interactive "P")
  (search-files-for-string-or-definition
   (or (thing-at-point 'symbol) (error "No word at point"))
   search-for-definition-p))

;;;###autoload
(defun search-files-by-name ()
  "Search for file names matching regex entered in minibuffer."
  (interactive)
  (search-files
   (read-from-minibuffer "File name regex: ")
   (projectile-project-root)
   'name))

(defun search-files-project-backend ()
  "Return search backend for project.

   Like `projectile-project-vcs', but git is the only VCS
   possibility, and does not search up the tree beyond the
   project root but instead defaults to ag."
  (if (file-exists-p (expand-file-name ".git" (projectile-project-root)))
      'git-grep
    'ag))

(defun search-files-for-string-or-definition (string search-for-definition-p)
  (search-files
   (if search-for-definition-p
       (search-files-get-definition-regex
        search-for-definition-p string major-mode)
     string)
   (projectile-project-root)
   (search-files-project-backend)))

(defun search-files-get-definition-regex (arg string major-mode)
  "Regular expression matching function/class etc definition for `string'."
  (let ((suffix (if (equal arg '(4)) "[^ (]*" "")))
    (case major-mode
      ('python-mode
       (format
        "\\(def\\|class\\) \\+%s%s(" string suffix))
      ('emacs-lisp-mode
       (format "(defun \\+%s%s \\+(" string suffix))
      (t
       (message "No definition regex for major mode %s; assuming python was intended" major-mode)
       (search-files-get-definition-regex arg string 'python-mode)))))

(defun search-files (string directory backend)
  (switch-to-buffer search-files-results-buffer-name)
  (delete-other-windows)
  (setq default-directory directory)
  (let ((buffer-read-only nil))
    (delete-region (point-min) (point-max))
    (save-excursion
      (insert (search-files-do-search string backend))))
  (filter-results-mode)
  (when (eq (count-lines (point-min) (point-max)) 1)
    (compile-goto-error)))

(defun search-files-do-search (string backend)
  (case backend
    ((ag git-grep)
     (filter-results-truncate-lines
      (shell-command-to-string
       (search-files-make-search-command string backend))))
    ('name
     (replace-regexp-in-string
      "$" ":1:"
      (filter-results-truncate-lines
       (shell-command-to-string
        (format "git ls-files | grep '%s'" string)))))
    (t (error "Invalid backend"))))

(defun search-files-make-search-command (string backend)
  (mapconcat
   #'shell-quote-argument
   (case backend
     ('ag
      (append '("ag")
              search-files-ag-arguments
              (list string ".")))
     ('git-grep
      (list "git" "grep" "-n" "--exclude-standard" "--no-index" string))
     (t (error "Invalid backend")))
   " "))

(provide 'search-files)
;;; search-files.el ends here
