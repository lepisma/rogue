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

;;;###autoload
(defun helm-elfeed ()
  (interactive)
  (helm :sources (helm-build-sync-source "Elfeed filters"
                   :candidates r-feeds-filters
                   :action 'r-feeds/elfeed-with-filter)
        :buffer "*helm elfeed*"))

(defun r-feeds/elfeed-with-filter (filter)
  "Run elfeed with given filter"
  (elfeed)
  (set-buffer "*elfeed-search*")
  (elfeed-search-set-filter filter))

;;;###autoload
(defun r-feeds/play-elfeed ()
  "Play the current link from elfeed in vlc, assuming the url
is something like youtube's."
  (interactive)
  (let ((entry (elfeed-search-selected :single)))
    (start-process "vlc" nil "vlc" (elfeed-entry-link entry))))

(provide 'r-feeds)

;;; r-feeds.el ends here
