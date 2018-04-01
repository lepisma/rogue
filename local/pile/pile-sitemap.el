;;; pile-sitemap.el --- Sitemap for pile -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/lepisma/pile-sitemap.el

;;; Commentary:

;; Sitemap for pile
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
(require 's)


(defun pile--fix-sitemap (list)
  "Walk over the list to remove index.org items"
  (let ((ignore-patterns '("/index.org"
                           "allpages.org"
                           "org-test.org")))
    (->> list
       (-remove (lambda (it)
                  (and (consp it) (= (length it) 1)
                       (-any (-cut s-contains? <> (car it)) ignore-patterns))))
       (-map (lambda (it) (if (consp it) (pile--fix-sitemap it) it))))))

(defun pile-sitemap (title list)
  (concat "#+TITLE: Sitemap\n\n" (org-list-to-org (pile--fix-sitemap list))))

(defun pile-sitemap-entry (entry style project)
  (cond ((not (directory-name-p entry))
         (format "[[file:%s][%s]]"
                 entry
                 (org-publish-find-title entry project)))
        ((eq style 'tree)
         (let ((index-file (f-join entry "index.org")))
           (format "[[file:%s][%s]]"
                   index-file
                   (org-publish-find-title index-file project))))
        (t entry)))

(provide 'pile-sitemap)

;;; pile-sitemap.el ends here
