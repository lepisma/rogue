;;; r-feeds.el --- Feed related functions -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Feed related functions
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

(require 'elfeed)
(require 'helm)
(require 'org)

(defcustom r-feeds-filters
  '(("Default" . "@6-months-ago +unread"))
  "alist of filters (identifier . filter) for elfeed."
  :type '(alist :key-type string
                :value-type string))

(defcustom r-feeds-dump-file nil
  "File to dump elfeed entries in."
  :type 'file)

;;;###autoload
(defun helm-elfeed ()
  (interactive)
  (helm :sources (helm-build-sync-source "Elfeed filters"
                   :candidates r-feeds-filters
                   :action 'r-feeds/elfeed-with-filter)
        :buffer "*helm elfeed*"))

(defun r-feeds/elfeed-with-filter (filter)
  "Run elfeed with given FILTER"
  (elfeed)
  (set-buffer "*elfeed-search*")
  (elfeed-search-set-filter filter))

(defun r-feeds/elfeed-entries (filter)
  "Return a list of entries for given FILTER"
  (let ((items)
        (filter (elfeed-search-parse-filter filter)))
    (with-elfeed-db-visit (entry feed)
      (if (elfeed-search-filter filter entry feed)
          (push entry items)))
    (sort items (lambda (a b) (> (elfeed-entry-date a) (elfeed-entry-date b))))))

(defun r-feeds/elfeed-to-org (output-file)
  "Dump elfeed entries for current view in OUTPUT-FILE."
  (interactive
   (list (or r-feeds-dump-file (read-file-name "Output file: " nil nil nil "elfeed-dump.org"))))
  (with-current-buffer (find-file-noselect output-file)
    (delete-region (point-min) (point-max))
    (insert "#+TITLE: Elfeed dump [" elfeed-search-filter "]\n")
    (insert (format-time-string "Last updated [%Y-%m-%d %a %H:%M]\n\n"))
    (let ((entries (r-feeds/elfeed-entries elfeed-search-filter)))
      (dolist (entry entries)
        (insert "* [[" (elfeed-entry-link entry) "][" (elfeed-entry-title entry) "]]\n")
        (org-set-tags-to (mapcar #'symbol-name (elfeed-entry-tags entry)))
        (org-set-property "FEED" (elfeed-feed-title (elfeed-entry-feed entry)))
        (insert "\n")))
    (save-buffer)))

;;;###autoload
(defun r-feeds/play-elfeed ()
  "Play the current link from elfeed in vlc, assuming the url
is something like youtube's."
  (interactive)
  (let ((entry (elfeed-search-selected :single)))
    (start-process "vlc" nil "vlc" (elfeed-entry-link entry))))

(provide 'r-feeds)

;;; r-feeds.el ends here
