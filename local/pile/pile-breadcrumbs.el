;;; pile-breadcrumbs.el --- Breadcrumbs for pile -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/lepisma/pile.el

;;; Commentary:

;; Breadcrumbs for pile
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


(require 'org)
(require 'ox)
(require 'f)
(require 's)

(defcustom pile-breadcrumbs-root-file nil
  "Path to the home file for breadcrumbs")

(defun pile--get-relative-path (file-name root-path)
  "Get relative path of the file from pile root"
  (let ((rel-path (f-relative file-name pile-source)))
    (s-chop-suffixes '(".org" "/index") rel-path)))

(defun pile--break-parents (rel-path)
  "Break path into roots"
  (let ((parents (cdr (reverse (s-split "/" rel-path)))))
    (reverse
     (-map-indexed (lambda (index item)
                     (let ((item-path))
                       (cons item
                             (concat (s-repeat (+ 1 index) "../") item "/index.html"))))
                   parents))))

(defun pile--linkify-parents (parents)
  "Return concatenated html for parents"
  (funcall 's-join "/" (-map (lambda (parent)
                               (format "<a href='%s'>%s</a>" (cdr parent) (car parent)))
                             parents)))

(defun pile--linkify-root (parents)
  "Return link for root file"
  (let ((root-url (concat (s-repeat (+ 1 (length parents)) "../") pile-breadcrumbs-root-file)))
    (format "<a href='%s'>%s</a>" root-url "≡")))

(defun pile--generate-breadcrumbs (rel-path)
  "Generate html breadcrumbs"
  (let* ((parents (pile--break-parents rel-path))
         (page-title (car (reverse (s-split "/" rel-path)))))
    (format "#+HTML:<div id='breadcrumbs'>%s/%s/%s</div>"
            (pile--linkify-root parents)
            (pile--linkify-parents parents)
            page-title)))

(defmacro with-pile-breadcrumbs (body)
  `(let ((org-export-before-parsing-hook))))

(provide 'pile-breadcrumbs)

;;; pile-breadcrumbs.el ends here
