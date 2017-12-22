;;; etab.el --- Web link clipping helper

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash "2.13.0") (dash-functional "2.13.0") (f "0.19.0") org-cliplink (s "1.12.0") stem-english (emacs "25"))
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
(require 'f)
(require 'org)
(require 'org-cliplink)
(require 'org-element)
(require 's)
(require 'stem-english)

(defcustom etab-bookmarks-file (f-full "~/etab.org")
  "File for saving bookmarks in")

(defcustom etab-ignore-words
  '("www" "http" "https" "com" "edu" "org" "io")
  "List of words to ignore while tokenizing")

(defun etab--tokenize-text (text)
  "Return a sorted list of tokens from text"
  (->> text
     (s-split-words)
     (-map #'downcase)
     (-remove (-cut -contains? etab-ignore-words <>))
     (-remove (lambda (w) (<= (length w) 2)))
     (-map #'stem-english)
     (-flatten)
     (-filter (lambda (w) (zerop (string-to-number w))))
     (-uniq)
     (-sort #'s-less?)))

(defun etab--compare (tokens-a tokens-b)
  "Find a basic similarity metric between two set of tokens."
  (/ (length (-filter (-cut -contains? tokens-b <>) tokens-a))
     (* 1.0 (min (length tokens-a) (length tokens-b)))))

(defun etab--compare-group (tokens tokens-group)
  "Return mean similarity between the given TOKENS list and items in TOKENS-GROUP"
  (let ((scores (-map (-cut etab-compare tokens <>) tokens-group)))
    (/ (-sum scores) (length scores))))

(defun etab--classify (tokens tokens-groups)
  (let ((similarities (-map (-cut etab--compare-group tokens <>) tokens-groups)))
    (car (--max-by (> (cdr it) (cdr other)) (--map-indexed (cons it-index it) similarities)))))

(defun etab--get-group-nodes (root-node)
  "Return level 1 headers from the ROOT-NODE as groups."
  (org-element-map root-node 'headline
    (lambda (node)
      (if (= (org-element-property :level node) 1)
          node
        nil)) nil nil t))

(defun etab--get-link-ranges (group-node)
  "Return range of link data from the group"
  (org-element-map group-node 'link
    (lambda (node)
      (if (-contains? '("https" "http") (org-element-property :type node))
          (cons (org-element-property :begin node) (org-element-property :end node))
        nil))))

(defun etab--get-group-tokens (group-node full-text)
  (let ((link-ranges (etab--get-link-ranges group-node)))
    (-map (lambda (range)
            (etab--tokenize-text
             (substring-no-properties full-text (car range) (cdr range)))) link-ranges)))

(defun etab--create-link-text (url title)
  "Text to be inserted for a link"
  (format "[[%s][%s]]" url (or title url)))

(defun etab--insert-link (buffer heading-position link-text)
  "Insert LINK under given HEADING in BUFFER"
  (save-excursion
    (with-current-buffer buffer
      (goto-char heading-position)
      (search-forward "\n")
      (insert "- ")
      (insert link-text)
      (insert "\n")
      (save-buffer))))

(defun etab-save-link (url title)
  "Add link to bookmarks file"
  (with-current-buffer (find-file-noselect etab-bookmarks-file)
    (let* ((tokens (etab--tokenize-text (s-concat url " " title)))
           (group-nodes (etab--get-group-nodes (org-element-parse-buffer)))
           (full-text (buffer-string))
           (target-idx (etab--classify tokens (-map (-cut etab--get-group-tokens <> full-text) group-nodes)))
           (target-node (nth target-idx group-nodes))
           (link-text (etab--create-link-text url title)))
      (etab--insert-link (current-buffer) (org-element-property :begin target-node) link-text))))

(defun etab-cliplink ()
  (interactive)
  (let ((url (substring-no-properties (current-kill 0))))
    (org-cliplink-retrieve-title url #'etab-save-link)))

(provide 'etab)

;;; etab.el ends here
