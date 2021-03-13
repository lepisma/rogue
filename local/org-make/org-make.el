;;; org-make.el --- Make like task runner based on README.org

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Make like task runner based on README.org
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
(require 'ob)
(require 'dash)
(require 'dash-functional)
(require 'helm)
(require 'projectile)

(defcustom org-make-file-name "README.org"
  "The main file to look tasks in.")

(defcustom org-make-task-prefix "om-"
  "Prefix to filter each task specified in org file with.")

(defcustom org-make-use-projectile nil
  "Flag specifying whether to use projectile for finding root dir.")

(defun org-make-get-dir ()
  "Get the main README.org file in the project"
  (if org-make-use-projectile
      (projectile-project-root)
    (locate-dominating-file "." org-make-file-name)))

(defmacro org-make-run-in-context (&rest body)
  `(let ((default-directory (org-make-get-dir)))
     (save-window-excursion
       (with-current-buffer (find-file-noselect org-make-file-name)
         ,@body))))

(defun org-make-tasks ()
  "Return a list of tasks"
  (org-make-run-in-context
   (-filter (-cut string-prefix-p org-make-task-prefix <>) (org-babel-src-block-names))))

(defun org-make-run-task (task-name)
  "Run TASK-NAME."
  (org-make-run-in-context
   (org-babel-goto-named-src-block task-name)
   (org-babel-execute-src-block)
   (save-buffer)))

;;;###autoload
(defun org-make ()
  "Run tasks from the project's file"
  (interactive)
  (helm :sources (helm-build-sync-source "org-make tasks in current project"
                   :candidates (lambda () (org-make-tasks))
                   :action 'org-make-run-task)
        :buffer "*helm org-make*"
        :prompt "task: "))

(provide 'org-make)

;;; org-make.el ends here
