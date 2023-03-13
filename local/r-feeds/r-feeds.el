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
(require 'request)

(defcustom r-feeds-raindrop-token nil
  "Authorization token for Raindrop.io API. This is used for saving
bookmarks to raindrop."
  :type 'string)

(defcustom r-feeds-filters
  '(("Default" . "@6-months-ago +unread"))
  "alist of filters (identifier . filter) for elfeed."
  :type '(alist :key-type string
                :value-type string))

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

(defun r-feeds/save-to-raindrop ()
  "Save current elfeed-search view entry to raindrop with a few
default options."
  (interactive)
  (let ((entry (elfeed-search-selected :single)))
    (request "https://api.raindrop.io/rest/v1/raindrop"
      :type "POST"
      :headers `(("Authorization" . ,(concat "Bearer " r-feeds-raindrop-token))
                 ("Content-Type" . "application/json"))
      :data (json-encode `(("pleaseParse" . #s(hash-table))
                           ("tags" . ("elfeed"))
                           ("link" . ,(elfeed-entry-link entry))))
      :success
      (cl-function (lambda (&key data &allow-other-keys)
                     (message "Saved item to Raindrop.io")
                     (elfeed-untag entry 'unread)
                     (elfeed-search-update-entry entry)))
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "Got error: %S" error-thrown))))))

(defun r-feeds/elfeed-entries (filter)
  "Return a list of entries for given FILTER"
  (let ((items)
        (filter (elfeed-search-parse-filter filter)))
    (with-elfeed-db-visit (entry feed)
      (if (elfeed-search-filter filter entry feed)
          (push entry items)))
    (sort items (lambda (a b) (> (elfeed-entry-date a) (elfeed-entry-date b))))))

(provide 'r-feeds)

;;; r-feeds.el ends here
