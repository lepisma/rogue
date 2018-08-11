;;; r-kv.el --- Key value secret management system -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))

;;; Commentary:

;; Key value secret management system
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defcustom r-kv-file nil
  "Path to data file")

(defun r-kv--write-to (alist file)
  (with-temp-file file
    (pp alist (current-buffer))))

(defun r-kv--write (alist)
  (if r-kv-file
      (r-kv--write-to alist r-kv-file)
    (message "No kv file set")))

(defun r-kv--read ()
  (if r-kv-file
      (with-current-buffer (find-file-noselect r-kv-file)
        (goto-char (point-min))
        (read (current-buffer)))
    (message "No kv file set")))

(defun r-kv--save-template (alist)
  (if r-kv-file
      (let ((template-file (concat r-kv-file ".template")))
        (r-kv--write-to alist template-file))
    (message "No kv file set")))

(defun r-kv--update (key value alist)
  (r-kv--save-template (cons (cons key nil) alist))
  (r-kv--write (cons (cons key value) alist)))

(defun r-kv-get (key)
  (let ((alist (r-kv--read)))
    (if (assoc key alist)
        (cdr (assoc key alist))
      (message "key not found, updating template")
      (r-kv--update key nil alist))))

(defun r-kv-set (key value)
  (let ((alist (r-kv--read)))
    (if (assoc key alist)
        (r-kv--write (setcdr (assoc key alist) value))
      (message "key not found, updating files")
      (r-kv--update key value alist))))

(provide 'r-kv)

;;; r-kv.el ends here
