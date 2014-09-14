;;; package --- Open URL of Github for current file
;;; Commentary:
;;; Code:
(defun open-github:chomp (str)
  (replace-regexp-in-string "[\n\r]+$" "" str))

(defun open-github:get-remote-repository-url ()
  (if (buffer-file-name)
      (progn
        (cd (file-name-directory (buffer-file-name)))
        (let*
            ((remote-raw (shell-command-to-string "git config --get remote.origin.url"))
             (remote     (open-github:chomp remote-raw)))
          (if (string-match "^[^@]+@\\([^:]+\\):\\([^/]+\\)/\\([^\\.]+\\)\\.git$" remote)
              (let
                  ((host (match-string 1 remote))
                   (user (match-string 2 remote))
                   (repo (match-string 3 remote)))
                (format "https://%s/%s/%s" host user repo)))))))

(defun open-github:open-repository ()
  "Open repository top page on Github for current file."
  (interactive)
  (let ((repo-url (open-github:get-remote-repository-url)))
    (if repo-url
        (browse-url repo-url)
      (message "It seems not a file on github."))))

(defun open-github:open-file ()
  "Open file page on Github for current file.
If region is selected, open with line parameters.")

(defun open-github:open-diff ()
  "Open diff page on Github for current file.
If region is selected, open with line parameters.")

(defun open-github:open-blame ()
  "Open blame page on Github for current file.
If region is selected, open with line parameters.")

(defun open-github:open-history ()
  "Open history page on Github for current file.")

(defun open-github:open-pull-request ()
  "Open pull-request top page on Github for current file.")

(provide 'open-github)
;;; open-github ends here
