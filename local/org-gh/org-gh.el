;;; org-gh.el --- Org mode github integration

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/lepisma/org-gh.el

;;; Commentary:

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

(require 'org)
(require 'thingatpt)

(defun org-gh-id-correct? (id)
  "Check whether the id is correct"
  (and id (not (zerop (string-to-number id)))))

(defun org-gh-open ()
  "Open gh id at point"
  (interactive)
  (let ((id (thing-at-point 'word)))
    (if (org-gh-id-correct? id)
        (org-gh-open-id id)
      (message "Nothing found at point"))))

(defun org-gh-current-repo ()
  "Look back and tell which repo we are talking about"
  (org-entry-get (point) "github" t))

(defun org-gh-build-url (repo-url id)
  "Build the url to open"
  (format "%s/issues/%s" repo-url id))

(defun org-gh-open-id (id)
  "Open gh id"
  (interactive "nId: ")
  (let ((repo (org-gh-current-repo)))
    (if repo
        (browse-url (org-gh-build-url repo id))
      (message "No github repo found looking back from point"))))

(provide 'org-gh)

;;; org-gh.el ends here
