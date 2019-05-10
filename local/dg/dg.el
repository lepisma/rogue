;;; dg.el --- Diagrams and stuff -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))

;;; Commentary:

;; Diagrams and stuff
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

(require 'cl-lib)
(require 'dash)
(require 'dash-functional)
(require 'helm)
(require 'web-server)

(defcustom dg-port 9100
  "Port for the visualization server")

(defvar dg-types '(fishbone)
  "Diagram types available.")

(defun dg-parse-subtree ()
  "Parse current org subtree"
  (org-narrow-to-subtree)
  (prog1 (org-element-parse-buffer)
    (widen)))

(defun dg--first-content (ast)
  (car (org-element-contents ast)))

(defun dg-fishbone-parse-plain-list (ast)
  (let ((items (org-element-contents ast)))
    (mapcar (lambda (it)
              (->> it
                 (funcall (-iteratefn #'dg--first-content 2))
                 substring-no-properties string-trim))
            items)))

(defun dg-fishbone-parse-headline (ast)
  (let ((title (org-element-property :raw-value ast)))
    `(,title ,@(mapcar #'dg-fishbone-parse-tree (org-element-contents ast)))))

(defun dg-fishbone-parse-tree (ast)
  "Make tree from parsed subtree for fishbone diagrams. At the
moment, we handle `headline' and `item' items."
  (cl-case (org-element-type ast)
    (org-data (dg-fishbone-tree (dg--first-content ast)))
    (headline (dg-fishbone-parse-headline ast))
    ;; Assuming that section will only have plain-list for now
    (section (dg-fishbone-parse-plain-list (dg--first-content ast)))
    (plain-list (dg-fishbone-parse-plain-list ast))))

(defun dg-fishbone-generate ()
  ;; TODO: Generate proper html with gojs plot
  (pp (dg-fishbone-parse-tree (dg-parse-subtree))))

;;;###autoload
(defun dg-plot (dg-type)
  (interactive (list
                (helm :sources (helm-build-sync-source "diagram type"
                                 :candidates dg-types)
                      :buffer "*helm dg diagram choice*"
                      :prompt "Digram Type: ")))
  (let ((fn (cl-ecase (intern dg-type)
              (fishbone #'dg-fishbone-generate))))
    (ws-start
     (lambda (req)
       (with-slots (process headers) req
         (ws-response-header process 200 '("Content-type" . "text/html"))
         (process-send-string process (funcall fn))))
     dg-port)))

(provide 'dg)

;;; dg.el ends here

