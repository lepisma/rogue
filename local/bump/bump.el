;;; bump.el --- Version bumper for projects -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/lepisma/bump.el

;;; Commentary:

;; Version bumper for projects. Assumes semver.
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

(require 'f)
(require 'json)
(require 'magit)
(require 'dash)

(defmacro with-project (&rest body)
  `(let ((root-path (projectile-project-root)))
     ,@body))

(defun bump-new-version (version bump-type)
  "Return new version for the BUMP-TYPE"
  (-let [(major minor patch) version]
    (cond ((eq bump-type 'patch) (list major minor (+ 1 patch)))
          ((eq bump-type 'minor) (list major (+ 1 minor) patch))
          ((eq bump-type 'major) (list (+ 1 major) minor patch))
          (t (message "lul what?")))))

(defun bump-detect-project ()
  "Detect which type of project this is"
  (with-project
   (cond ((f-exists? (f-join root-path "package.json")) 'js)
         ((f-exists? (f-join root-path "setup.py")) 'python)
         (t (message "kek")))))

(defun bump-parse-version (version-string)
  (s-split "." verson-string))

(defun bump-unparse-version (version)
  (s-join "." (-map #'number-to-string version)))

(defun bump-get-js ()
  "Parse version for js project"
  (with-project
   (->> (f-join root-path "package.json")
      (f-read-text)
      (json-read-from-string)
      (assoc 'version)
      (cdr))))

(defun bump-set-js (version-string)
  "Set version for js project"
  (with-project
   (shell-command-to-string (format "npm version %s" version-string))))

(provide 'bump)

;;; bump.el ends here
