;;; kindle.el --- Kindle utilities -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/lepisma/kindle.el

;;; Commentary:

;; Functions for interacting with kindle
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
(require 'f)
(require 's)

(defconst kindle-mount-path "/proc/mounts")

(defun kindle-read-clippings-file ()
  "Read clippings file from mounted kindle"
  (let ((mount-line (->> (f-read-text kindle-mount-path)
                       (s-split "\n")
                       (-find (-cut s-contains? "Kindle" <>)))))
    (if mount-line
        (f-read-text (f-join (second (s-split " " mount-line))
                             "documents"
                             "My Clippings.txt"))
      (message "Kindle not mounted"))))

(defun kindle-parse-clipping (clipping)
  "Parse a clipping text"
  (let* ((lines (s-split "\n" (s-trim clipping)))
         (source (s-trim (car lines)))
         (added (->> (s-split "| Added on " (second lines)) (second) (s-trim)))
         (text (s-trim (s-join "\n" (nthcdr 2 lines)))))
    (list text source added)))

(defun kindle-parse-clippings-text (text)
  "Parse a list of clippings from the text"
  (let ((clippings (s-split "==========" (s-trim text))))
    (-map #'kindle-parse-clipping
          (-remove (-cut string-equal <> "") clippings))))

(provide 'kindle)

;;; kindle.el ends here
