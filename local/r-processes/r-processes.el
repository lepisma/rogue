;;; r-processes.el --- Process management for rogue

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/lepisma/rogue/tree/master/local/r-processes

;;; Commentary:

;; Personal config package for working with processes
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

(require 'prodigy)
(require 'dash)
(require 'dash-functional)

(defcustom r-processes-git-update-dirs '()
  "git directories to autoupdate.")

;;;###autoload
(defun r-processes-define (name &optional args)
  "Define a basic prodigy service."
  (prodigy-define-service
    :name name
    :command name
    :args args
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t))

;;;###autoload
(defun r-processes-start-service (name)
  "Start a service by name."
  (let ((service (-first (lambda (s) (string-equal name (lax-plist-get s :name))) prodigy-services)))
    (if service
        (prodigy-start-service service)
      (message "No service found"))))

(defun r-processes-git-autoupdate (project-dir)
  "Add all, commit and push given projects."
  (let ((default-directory project-dir))
    ;; Blocking add
    (shell-command-to-string "git add .")
    ;; Using this to avoid gpg tty issue + to use settings from emacs
    (ignore-errors
      (magit-commit '("-m" "git-auto-update")))
    (call-process-shell-command "git push" nil 0)))

;;;###autoload
(defun r-processes-run-git-autoupdate-loop (itime gtime)
  (run-at-time
   itime gtime
   (lambda ()
     (dolist (project-dir r-processes-git-update-dirs)
       (r-processes-git-autoupdate project-dir)))))

(provide 'r-processes)

;;; r-processes.el ends here
