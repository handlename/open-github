;;; package --- Open URL of Github for current file
;;; Commentary:
;;; Code:
(defun open-github:open-repository ()
  "Open repository top page on Github for current file.")

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
