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

(defcustom kindle-clipping-format
  "#+BEGIN_QUOTE
%s

#+HTML:<footer>%a</footer>
#+END_QUOTE"
  "Format for pretty printing a clipping.
- %s is replaced by the main text
- %t is for date (and time) added
- %a is for book and author")

(defun kindle-check-device ()
  "Check if kindle is mounted"
  (let ((mount-line (->> (f-read-text kindle-mount-path)
                       (s-split "\n")
                       (-find (-cut s-contains? "Kindle" <>)))))
    mount-line))

(defun kindle-read-clippings-file (mount-directory)
  "Read clippings file from the kindle mount directory"
  (f-read-text (f-join mount-directory "documents" "My Clippings.txt")))

(defun kindle-parse-clipping (clipping-text)
  "Parse a clipping text"
  (let* ((lines (s-split "\n" (s-trim clipping-text)))
         (source (s-trim (car lines)))
         (added (->> (s-split "| Added on " (second lines)) (second) (s-trim)))
         (text (s-trim (s-join "\n" (nthcdr 2 lines)))))
    (list text source added)))

(defun kindle-parse-clippings-text (text)
  "Parse a list of clippings from the text"
  (let ((clippings (s-split "==========" (s-trim text))))
    (-map #'kindle-parse-clipping
          (-remove (-cut string-equal <> "") clippings))))

(defun kindle-pretty-print-clipping (clipping)
  "Pretty print a clipping using the format specified in kindle-clipping-format"
  (s-replace-all '(("%s" . (nth 0 clipping))
                   ("%a" . (nth 1 clipping))
                   ("%t" . (nth 2 clipping)))
                 kindle-clipping-format))

(provide 'kindle)

;;; kindle.el ends here
