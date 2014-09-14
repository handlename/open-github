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

(defun open-github:get-branch ()
  (let* ((symbolic-ref-raw (shell-command-to-string "git symbolic-ref HEAD"))
         (symbolic-ref     (open-github:chomp symbolic-ref-raw)))
    (string-match "^refs/heads/\\(.+\\)$" symbolic-ref)
    (match-string 1 symbolic-ref)))

(defun open-github:get-file-path ()
  (let* ((top-path-raw (shell-command-to-string "git rev-parse --show-toplevel"))
         (top-path     (open-github:chomp top-path-raw))
         (abs-path     (buffer-file-name)))
    (replace-regexp-in-string (format "^%s/" top-path) "" abs-path)))

(defun open-github:make-line-params (begin end)
  (if (use-region-p)
      (let
          ((begin-num (line-number-at-pos begin))
           (end-num   (line-number-at-pos end)))
        (if (= begin-num end-num)
            (format "#L%d" begin-num)
          (format "#L%d-%d" begin-num end-num)))
    ""))

(defun open-github:open-url (format &optional params)
  (let ((repo-url (open-github:get-remote-repository-url)))
    (if repo-url
        (let ((branch    (open-github:get-branch))
              (file-path (open-github:get-file-path)))
          (browse-url (format format repo-url branch file-path params)))
      (message "It seems not a file on github."))))

(defun open-github:open-repository ()
  "Open repository top page on Github for current file."
  (interactive)
  (let ((repo-url (open-github:get-remote-repository-url)))
    (if repo-url
        (browse-url repo-url)
      (message "It seems not a file on github."))))

(defun open-github:open-file (begin end)
  "Open file page on Github for current file.
If region is selected, open with line parameters."
  (interactive "r")
  (let ((line-params (open-github:make-line-params begin end)))
    (open-github:open-url "%s/blob/%s/%s%s" line-params)))

(defun open-github:open-diff ()
  "Open diff page on Github for current file.
If region is selected, open with line parameters.")

(defun open-github:open-blame ()
  "Open blame page on Github for current file.
If region is selected, open with line parameters.")

(defun open-github:open-history ()
  "Open history page on Github for current file."
  (interactive)
  (open-github:open-url "%s/commits/%s/%s"))

(defun open-github:open-pull-request ()
  "Open pull-request top page on Github for current file.")

(provide 'open-github)
;;; open-github ends here
