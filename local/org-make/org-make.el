;;; orgo.el --- Task runner based on Org-Mode documents

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Task runner based on Org-Mode documents
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
(require 'org)
(require 'ob)
(require 'helm)
(require 'projectile)

(defcustom orgo-file-name "README.org"
  "The main file to look tasks in.")

(defcustom orgo-task-prefix "om-"
  "Prefix to filter each task specified in org file with.")

(defcustom orgo-use-projectile nil
  "Flag specifying whether to use projectile for finding root dir.")

(defun orgo-get-dir ()
  "Get the main README.org file in the project"
  (if orgo-use-projectile
      (projectile-project-root)
    (locate-dominating-file "." orgo-file-name)))

(defmacro orgo-run-in-context (&rest body)
  `(let ((default-directory (orgo-get-dir)))
     (save-window-excursion
       (with-current-buffer (find-file-noselect orgo-file-name)
         ,@body))))

(defun orgo-tasks ()
  "Return a list of tasks"
  (orgo-run-in-context
   (remove-if-not
    (lambda (name) (string-prefix-p orgo-task-prefix name))
    (org-babel-src-block-names))))

(defun orgo-run-task (task-name)
  "Run TASK-NAME."
  (orgo-run-in-context
   (org-babel-goto-named-src-block task-name)
   (org-babel-execute-src-block t)
   (save-buffer)))

;;;###autoload
(defun orgo ()
  "Run tasks from the project's file"
  (interactive)
  (let ((tasks (orgo-tasks)))
    (if tasks
        (helm :sources (helm-build-sync-source "orgo tasks in current project"
                         :candidates tasks
                         :action 'orgo-run-task)
              :buffer "*helm orgo*"
              :prompt "task: ")
      (error "No orgo tasks found for the current project."))))

(provide 'orgo)

;;; orgo.el ends here
