;;; search-files.el --- search files using git grep or other backends
(require 'projectile)

(defvar search-files-ag-arguments)

(defvar search-files-results-buffer-name "*search*")

(defvar search-files-mode-map (make-sparse-keymap))

(define-derived-mode search-files-mode
    compilation-mode "search-files"
  "Major mode for search-files results buffer.
\\{search-files-mode-map}"

  (add-hook 'compilation-finish-functions 'search-files-clean-up-compilation-buffer))

(define-key search-files-mode-map "/" 'search-files-delete-non-matching-lines)
(define-key search-files-mode-map [(control /)] 'search-files-undo)
(define-key search-files-mode-map "\C-_" 'search-files-undo)
(define-key search-files-mode-map "\C-xu" 'search-files-undo)
(define-key search-files-mode-map [(super z)] 'search-files-undo)

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

(defun search-files-delete-matching-lines ()
  "Filter search results by excluding matching lines"
  (interactive)
  (search-files-do-in-results-buffer 'delete-matching-lines))

(defun search-files-delete-non-matching-lines ()
  "Filter search results by retaining only matching lines"
  (interactive)
  (search-files-do-in-results-buffer 'delete-non-matching-lines))

(defun search-files-undo ()
  "Undo in search results buffer"
  (interactive)
  (search-files-do-in-results-buffer 'undo))

(defun search-files-do-in-results-buffer (fn)
  (let ((buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (call-interactively fn))))

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
    (search-files-mode)
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

(provide 'search-files)
