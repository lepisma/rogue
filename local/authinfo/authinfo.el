;;; authinfo.el --- Functions to read from ~/.authinfo.gpg

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/lepisma/rogue/tree/master/local/authinfo

;;; Commentary:

;; Functions to read from authinfo
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

(require 'dash)
(require 'dash-functional)
(require 's)

(defun authinfo-get-entries ()
  "Return entries from authinfo"
  (let* ((cmd-out (s-trim (shell-command-to-string "gpg --use-agent --quiet --batch -d ~/.authinfo.gpg")))
         (lines (-remove (-cut s-starts-with-p "#" <>) (s-split "\n" cmd-out))))
    (mapcar (-cut s-split " " <>) lines)))

(defun authinfo-get-match (name)
  "Return matching entry for given name identifier"
  (let ((entries (authinfo-get-entries)))
    (-find (lambda (entry) (string-equal (lax-plist-get entry "name") name)) entries)))

(defun authinfo-get-value (machine port key)
  (lax-plist-get (authinfo-get-match machine port) key))

(provide 'authinfo)

;;; authinfo.el ends here
