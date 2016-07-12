;;; search-files.el --- search files using `git grep` or other backends
;; Version: 0.0.0
;; Author: dandavison7@gmail.com
;; Package-Requires: ((projectile "0.10.0"))
;; Keywords: search, git, projectile
;; URL: https://github.com/dandavison/emacs-search-files

(require 'cl)
(require 'projectile)

(defvar search-files-ag-arguments '("-z"))

(defvar search-files-results-buffer-name "*search*")

(defvar search-files-mode-map (make-sparse-keymap))

(defvar search-files-max-line-width 300
  "Output lines will be truncated to this width to avoid slowing down emacs")

(define-derived-mode search-files-mode
  compilation-mode "search-files"
  "Major mode for search-files results buffer.
\\{search-files-mode-map}"

  (add-hook 'compilation-finish-functions
            'search-files-clean-up-compilation-buffer nil t))

(define-key search-files-mode-map "/" 'search-files-filter-results)
(define-key search-files-mode-map [(control /)] 'search-files-undo)
(define-key search-files-mode-map "\C-k" 'search-files-kill-line)
(define-key search-files-mode-map "\C-_" 'search-files-undo)
(define-key search-files-mode-map "\C-xu" 'search-files-undo)
(define-key search-files-mode-map [(super z)] 'search-files-undo)

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

(defun search-files-filter-results (&optional arg)
  "Filter search results, retaining matching lines.

With prefix argument, retain non-matching lines."
  (interactive "P")
  (search-files-do-in-results-buffer
   (if arg 'delete-matching-lines 'delete-non-matching-lines) 'from-beginning))

(defun search-files-kill-line ()
  "Kill line in search results buffer"
  (interactive)
  (search-files-do-in-results-buffer 'kill-line))

(defun search-files-undo ()
  "Undo in search results buffer"
  (interactive)
  (search-files-do-in-results-buffer 'undo))

(defun search-files-do-in-results-buffer (fn &optional do-from-beginning)
  (let ((buffer-read-only nil))
    (save-excursion
      (when do-from-beginning (goto-char (point-min)))
      (call-interactively fn))))

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
  (search-files-mode)
  (when (eq (count-lines (point-min) (point-max)) 1)
    (compile-goto-error)))

(defun search-files-do-search (string backend)
  (case backend
    ((ag git-grep)
     (search-files-truncate-lines
      (shell-command-to-string
       (search-files-make-search-command string backend))))
    ('name
     (replace-regexp-in-string
      "$" ":1:"
      (search-files-truncate-lines
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

(defun search-files-truncate-lines (string)
  "Truncates lines to `search-files-max-line-width'"
  (mapconcat
   (lambda (line) (truncate-string-to-width line search-files-max-line-width))
   (split-string string "[\n\r]+")
   "\n"))

(defun search-files-clean-up-compilation-buffer (&optional buf ignored)
  (with-current-buffer (or buf (current-buffer))
    (let ((buffer-read-only nil)
          (grep-match-re "^[^: ]+:[0-9]+:"))
      (save-excursion
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
                       (point))))))


(provide 'search-files)
;;; search-files.el ends here
