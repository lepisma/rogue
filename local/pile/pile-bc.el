;;; pile-bc.el --- Breadcrumbs for pile -*- lexical-binding: t; -*-

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

(defcustom pile-bc-root-file nil
  "Path to the home file for breadcrumbs")

(defun pile-bc--relative (file-name)
  "Get relative path of the file from pile root"
  (let ((rel-path (f-relative file-name pile-source)))
    (s-chop-suffix ".org" rel-path)))

(defun pile-bc--parents (rel-path)
  "Break path into roots"
  (let* ((splits (reverse (s-split "/" rel-path)))
         (dir-page? (string-equal (car splits) "index"))
         (offset (if dir-page? 2 1))
         (parents (nthcdr offset splits)))
    (reverse
     (-map-indexed (lambda (index item)
                     (let ((item-path))
                       (cons item
                             (concat (s-repeat (+ offset index) "../") item "/index.html"))))
                   parents))))

(defun pile-bc--linkify-parents (parents)
  "Return concatenated html for parents"
  (funcall 's-join " / " (-map (lambda (parent)
                                 (format "<a href='%s'>%s</a>" (cdr parent) (car parent)))
                               parents)))

(defun pile-bc--linkify-root (parents)
  "Return link for root file"
  (let ((root-url (concat (s-repeat (+ 1 (length parents)) "../") pile-bc-root-file)))
    (format "<a href='%s'>%s</a>" root-url "≡ index")))

(defun pile-bc-generate-breadcrumbs (rel-path)
  "Generate html breadcrumbs"
  (let* ((splits (reverse (s-split "/" rel-path)))
         (parents (pile-bc--parents rel-path))
         (parent-links (pile-bc--linkify-parents parents))
         (root-link (pile-bc--linkify-root parents))
         (page-title (if (string-equal (car splits) "index")
                         (if (null (second splits)) "home" (second splits))
                       (car splits))))
    (format "#+HTML:<div id='breadcrumbs'>%s / %s %s</div>"
            root-link
            (if (zerop (length parents)) "" (format "%s /" parent-links))
            page-title)))

(defun pile-bc-hook (_)
  "Function to insert breadcrumbs in the exported file"
  (search-forward ".setup")
  (next-line)
  (let ((rel-path (pile-bc--relative (buffer-file-name))))
    (insert (pile-bc-generate-breadcrumbs rel-path))))

(defmacro with-pile-bc (body)
  "Run body with pile bc export hook set"
  (let ((remove-form '(remove-hook 'org-export-before-parsing-hook 'pile-bc-hook)))
    `(condition-case err
         (progn
           (add-hook 'org-export-before-parsing-hook 'pile-bc-hook)
           ,body
           ,remove-form)
       (error (progn
                ,remove-form
                (print err))))))

(provide 'pile-bc)

;;; pile-bc.el ends here
