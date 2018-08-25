;;; obtt.el --- Org babel tangle templates -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/lepisma/obtt.el

;;; Commentary:

;; Org babel tangle templates
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
(require 'ob-tangle)
(require 'cl-lib)


(defun obtt-parse-args (args-string)
  (split-string (string-trim args-string)))

(defun obtt-all-files ()
  "Return all the output files involved in the template."
  (org-element-map (org-element-parse-buffer) 'src-block
    (lambda (blk)
      (let ((params (org-element-property :parameters blk)))
        (alist-get :tangle (org-babel-parse-header-arguments params))))))

(defun obtt-prepare-directories (files)
  (dolist (file files)
    (mkdir (file-name-directory (directory-file-name file)) t)))

(defun obtt-eval-blocks ()
  "Run all blocks marked with `:obtt eval'"
  (org-babel-map-src-blocks nil
    (let* ((obtt-args-string (alist-get :obtt (org-babel-parse-header-arguments header-args)))
           (obtt-args (if obtt-args-string (obtt-parse-args obtt-args-string))))
      (if (member "eval" obtt-args)
          (org-babel-execute-src-block)))))

(defun obtt-available-snippets ()
  "Look for all available obtt snippets"
  (cl-reduce (lambda (acc d)
               (if (stringp d)
                   (let ((dir (concat (file-name-as-directory d) "org-mode")))
                     (if (file-exists-p dir)
                         (append acc (directory-files dir nil "^obtt"))
                       acc))
                 acc))
             yas-snippet-dirs :initial-value nil))

;;;###autoload
(defun obtt-tangle ()
  (interactive)
  (let ((files (obtt-all-files)))
    (obtt-prepare-directories files)
    (org-babel-tangle)
    (obtt-eval-blocks)))

;;;###autoload
(defun obtt-new (directory)
  (interactive "DStarting directory: ")
  (let ((buffer (create-file-buffer (concat directory "obtt-seed"))))
    (switch-to-buffer buffer)
    (org-mode)
    (insert "# -*- mode:org -*-\n")
    (insert "# This is an obtt seed file, start with a snippet.\n# Here are the available snippets:\n")
    (dolist (snip (obtt-available-snippets))
      (insert (format "# - %s\n" snip)))
    (insert "\n")))

(provide 'obtt)

;;; obtt.el ends here
