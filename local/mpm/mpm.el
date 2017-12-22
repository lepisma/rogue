;;; mpm.el --- Iterface for mpm

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash "2.13.0") (org-cliplink) (s "1.12.0") (emacs "25"))
;; URL: https://github.com/lepisma/mpm

;;; Commentary:

;; Interfaces mpm with Emacs, allowing easy music clipping
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
(require 'org-cliplink)
(require 's)

(defun mpm-detect-metadata (page-title)
  "Try to figure out song metadata from PAGE-TITLE.
Return a cons cell like this (artist . title).
TODO: Save artists for better heuristic."
  (let ((parts (--> page-title
                  (s-split "-" it)
                  (butlast it)
                  (mapcar #'s-trim it)
                  (mapcar #'s-collapse-whitespace it))))
    (if (= (length parts) 2)
        ;; This is probably artist - title pair
        (apply #'cons parts)
      ;; This is weird, just let the user decide
      (let ((main-title (s-join "-" parts)))
        (cons main-title main-title)))))

(defun mpm-url-valid? (url)
  (let ((url-main (->> url
                     (s-chop-prefix "http://")
                     (s-chop-prefix "https://")
                     (s-chop-prefix "www."))))
    (or (s-starts-with? "youtube.com" url-main)
        (s-starts-with? "youtu.be" url-main))))

(defmacro mpm-run-import (url artist title &optional album)
  "Import the item in mpm"
  (let ((args `("--artist" ,artist
                "--title" ,title
                "--url" ,url)))
    `(start-process "*mpm*" (get-buffer-create "*mpm*") "mpm" "add"
                    ,@(if album
                          (append (list "--album" (shell-quote-argument album))
                                  args)
                        args))))

(defun mpm-confirm-metadata (url artist-title-pair)
  "Ask user for confirming the metadata provided"
  (let ((buffer (get-buffer-create "*mpm-confirm*")))
    (set-buffer buffer)
    (emacs-lisp-mode)
    (erase-buffer)
    (insert ";; Mpm import confirmation\n\n")
    (insert ";; Check artist and title, swap if needed.\n")
    (insert ";; Insert album as the last argument to mpm-run-import.\n\n")
    (insert (pp `(let ((artist ,(car artist-title-pair))
                       (title ,(cdr artist-title-pair))
                       (url ,url))
                   (mpm-run-import url artist title nil)
                   (bury-buffer))))
    (switch-to-buffer buffer)))

(defun mpm-add-link (url)
  (if (mpm-url-valid? url)
      (org-cliplink-retrieve-title
       url
       (lambda (url page-title)
         (mpm-confirm-metadata url (mpm-detect-metadata page-title))))
    (message "Not a valid url")))

;;;###autoload
(defun mpm-cliplink ()
  "Save the current link to mpm database."
  (interactive)
  (let ((url (substring-no-properties (current-kill 0))))
    (mpm-add-link url)))

(provide 'mpm)

;;; mpm.el ends here
