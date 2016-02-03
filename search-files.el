;;; search-files.el --- search files using git grep or other backends
(require 'projectile)

(defvar search-files-ag-arguments)

(defvar search-files-results-buffer-name "*search*")

(defun search-files-read-from-minibuffer (&optional search-for-definition-p)
  "Search for word entered in minibuffer.

  With prefix arg search for definition."
  (interactive "P")
  (search-files-for-string-or-definition
   (read-from-minibuffer "Regexp: ")
   search-for-definition-p))

(defun search-files-thing-at-point (&optional search-for-definition-p)
  "Search for word at point.

  With prefix arg search for definition."
  (interactive "P")
  (search-files-for-string-or-definition
   (or (thing-at-point 'symbol) (error "No word at point"))
   search-for-definition-p))

(defun search-files-for-string-or-definition (string search-for-definition-p)
  (search-files
   (if search-for-definition-p
       (search-files-get-definition-regex string major-mode)
     string)
   (projectile-project-root)))

(defun search-files-get-definition-regex (string major-mode)
  "Regular expression matching function/class etc definition for `string'."
  (case major-mode
    ('python-mode
     (format "\\(def\\|class\\) \\+%s(" string))
    ('emacs-lisp-mode
     (format "(defun %s \\+(" string))
    (t
     (error "No definition regex for major mode %s" major-mode))))

(defun search-files (string directory)
  (let ((backend (if (eq (projectile-project-vcs) 'git) 'git-grep 'ag)))
    (switch-to-buffer search-files-results-buffer-name)
    (delete-other-windows)
    (setq default-directory directory)
    (let ((buffer-read-only nil))
      (delete-region (point-min) (point-max))
      (save-excursion
        (insert (shell-command-to-string
                 (search-files-make-search-command string backend)))))
    (compilation-mode)
    (when (eq (count-lines (point-min) (point-max)) 1)
      (compile-goto-error))))

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

(defun search-files-clean-up-compilation-buffer (buf status)
  (with-current-buffer buf
    (let ((buffer-read-only nil)
          (grep-match-re "^[^: ]+:[0-9]+:"))
      (goto-char (point-min))
      (delete-region (point)
                     (progn
                       (re-search-forward grep-match-re)
                       (point-at-bol)))
      (goto-char (point-max))
      (delete-region (progn
                       (re-search-backward grep-match-re)
                       (forward-line 1)
                       (point-at-bol))
                     (point)))))

(add-hook 'compilation-finish-functions 'search-files-clean-up-compilation-buffer)

(provide 'search-files)
