;;; etab.el --- Web link clipping helper

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash "2.13.0") (dash-functional "2.13.0") org-cliplink (s "1.12.0") (emacs "25"))
;; URL: https://github.com/lepisma/etab.el

;;; Commentary:

;; etab provides helpful functions to save and organize web bookmarks
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
(require 'org-cliplink)
(require 's)

(defcustom etab-ignore-words
  '("www" "http" "https" "com" "edu" "org" "io")
  "List of words to ignore while tokenizing")

(defun etab-tokenize-text (text)
  "Return a sorted list of tokens from text"
  (->> text
     (s-split-words)
     (-map #'downcase)
     (-remove (-cut -contains? etab-ignore-words <>))
     (-remove (lambda (w) (< (length w) 2)))
     (-uniq)
     (-sort #'s-less?)))

(defun etab-compare (tokens-a tokens-b)
  "Find a basic similarity metric between two set of tokens."
  (/ (length (-filter (-cut -contains? tokens-b <>) tokens-a))
     (* 1.0 (min (length tokens-a) (length tokens-b)))))

(defun etab-compare-group (tokens tokens-group)
  "Return mean similarity between the given TOKENS list and items in TOKENS-GROUP"
  (let ((scores (-map (-cut etab-compare tokens <>) tokens-group)))
    (/ (-sum scores) (length scores))))

(defun etab-link-tokens (url title)
  "Return tokens from URL & TITLE to be used for categorizing"
  (->> (-map #'etab-tokenize-text `(,url ,title))
     (-flatten)
     (-uniq)
     (-sort #'s-less?)))

(defun etab-cliplink ()
  (interactive)
  (let ((url (substring-no-properties (current-kill 0))))
    (org-cliplink-retrieve-title url #'etab-link-properties)))

(provide 'etab)

;;; etab.el ends here
