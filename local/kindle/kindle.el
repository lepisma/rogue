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

(defcustom kindle-clipping-save-file nil
  "Save file for kindle clippings. Assumes a few things to be in
the file as of now.")

(defun kindle-check-device ()
  "Check if kindle is mounted"
  (let ((mount-line (->> (f-read-text kindle-mount-path)
                       (s-split "\n")
                       (-find (-cut s-contains? "Kindle" <>)))))
    mount-line))

(defun kindle--get-mount-path (mount-line)
  "Return mount path from /proc/mounts MOUNT-LINE"
  (second (s-split " " mount-line)))

(defun kindle-clipping--parse (clipping-text)
  "Parse a clipping from CLIPPING-TEXT"
  (let* ((lines (s-split "\n" (s-trim clipping-text)))
         (source (s-trim (car lines)))
         (added (->> (s-split "| Added on " (second lines)) (second) (s-trim)))
         (text (s-trim (s-join "\n" (nthcdr 2 lines)))))
    (list text source added)))

(defun kindle-clipping-read-file (mount-directory)
  "Parse a list of clippings from kindle MOUNT-DIRECTORY"
  (let* ((clippings-file (f-join mount-directory "documents" "My Clippings.txt"))
         (text (f-read-text clippings-file))
         (clippings (s-split "==========" (s-trim text))))
    (-map #'kindle-clipping--parse
          (-remove (-cut string-equal <> "") clippings))))

(defun kindle-clipping-clear-device-file (mount-directory)
  "Clear the clipping file in MOUNT-DIRECTORY"
  (let ((clippings-file (f-join mount-directory "documents" "My Clippings.txt")))
    (f-write-text "" 'utf-8 clippings-file)))

(defun kindle-clipping--pretty-print (clipping)
  "Pretty print a clipping using the format specified in kindle-clipping-format"
  (s-replace-all `(("%s" . ,(nth 0 clipping))
                   ("%a" . ,(nth 1 clipping))
                   ("%t" . ,(nth 2 clipping)))
                 kindle-clipping-format))

(defun kindle-clipping--in-buffer? (clipping buffer-text)
  "Check if CLIPPING is present in BUFFER-TEXT"
  (let ((text (s-collapse-whitespace (nth 0 clipping))))
    (s-contains? text buffer-text)))

(defun kindle-clipping--filter-clippings (clippings buffer)
  "Remove CLIPPINGS already present in BUFFER"
  (let ((buffer-text (s-collapse-whitespace
                      (with-current-buffer buffer
                        (buffer-substring-no-properties (point-min) (point-max))))))
    (-remove (-cut kindle-clipping--in-buffer? <> buffer-text) clippings)))

(defun kindle-clipping--insert-clipping (clipping buffer)
  "Insert CLIPPING at the top of BUFFER"
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (search-forward "div class=\"epigraph\">" nil t)
      (insert "\n")
      (insert (kindle-clipping--pretty-print clipping))
      (insert "\n")
      (search-backward "#+BEGIN_QUOTE" nil t)
      (next-line)
      (org-fill-element))))

(defun kindle-clipping-ingest-new ()
  "Ingest new clippings"
  (interactive)
  (let ((mount-line (kindle-check-device)))
    (if mount-line
        (let* ((buffer (find-file-noselect kindle-clipping-save-file))
               (clippings (--> (kindle--get-mount-path mount-line)
                            (kindle-clipping-read-file it)
                            (kindle-clipping-filter-clippings it buffer))))
          (message (format "Found %s new clippings" (length clippings)))
          (-each clippings (-cut kindle-clipping--insert-clipping <> buffer))
          (with-current-buffer buffer (save-buffer)))
      (message "Kindle not found"))))

(provide 'kindle)

;;; kindle.el ends here
