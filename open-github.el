;;; open-github.el --- Open URL of Github for current file

;; Copyright (C) 2014 by NAGATA Hiroaki

;; Author: NAGATA Hiroaki (@handlename)
;; URL: https://github.com/handlename/open-github
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is utilty to open URL of Github for current file in default browser.
;; To use tthis package, add these lines to your init.el or .emacs file:
;;     (add-to-list 'load-path "path/to/open-github-dir")
;;     (require 'open-github)

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
  (open-github:open-url "%s"))

(defun open-github:open-file (begin end)
  "Open file page on Github for current file.
If region is selected, open with line parameters."
  (interactive "r")
  (let ((line-params (open-github:make-line-params begin end)))
    (open-github:open-url "%s/blob/%s/%s%s" line-params)))

;;; TODO:
(defun open-github:open-diff ()
  "Open diff page on Github for current file.
If region is selected, open with line parameters."
  (interactive)
  (message "not implement yet"))

(defun open-github:open-blame (begin end)
  "Open blame page on Github for current file.
If region is selected, open with line parameters."
  (interactive "r")
  (let ((line-params (open-github:make-line-params begin end)))
    (open-github:open-url "%s/blame/%s/%s%s" line-params)))

(defun open-github:open-history ()
  "Open history page on Github for current file."
  (interactive)
  (open-github:open-url "%s/commits/%s/%s"))

;;; TODO:
(defun open-github:open-pull-request ()
  "Open pull-request top page on Github for current file."
  (interactive)
  (message "not implement yet"))

(provide 'open-github)
;;; open-github.el ends here
