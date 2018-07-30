;;; bmp.el --- Version bmper for projects -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/lepisma/bmp.el

;;; Commentary:

;; Version bmper for projects. Assumes semver.
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
(require 'helm)
(require 'projectile)

(require 'bmp-poetry)

(defcustom bmp-project-fns
  '(bmp-poetry-get-project)
  "Functions for getting projects")

(defun bmp ()
  "Bump version for current project."
  (interactive)
  (helm :sources (helm-build-sync-source "bump type"
                   :candidates '(("patch" . patch) ("minor" . minor) ("major" . major))
                   :action #'bmp-bump)
        :buffer "*helm bump*"))

(defun bmp-bump (bmp-type)
  "Bump version using the given bmp-type"
  (let ((default-directory (projectile-project-root)))
    (let ((project (bmp-get-project bmp-project-fns)))
      (if (null project)
          (message "No project detected")
        (let* ((version-str (bmp-get-version project))
               (new-ver-str (bmp-new-version version-str bmp-type)))
          (bmp-set-version project new-ver-str)
          (let ((affected-files (bmp-get-files project)))
            (bmp-commit affected-files new-ver-str)
            (bmp-tag new-ver-str)))))))

(defun bmp-get-project (fns)
  (unless (null fns)
    (or (funcall (car fns))
        (bmp-get-project (cdr fns)))))

(defun bmp-new-version (version-str bmp-type)
  "Return new version for the BMP-TYPE"
  (let ((version (bmp-parse-version version-str)))
    (-let [(major minor patch) version]
      (bmp-unparse-version
       (ecase bmp-type
         ('patch (list major minor (+ 1 patch)))
         ('minor (list major (+ 1 minor) 0))
         ('major (list (+ 1 major) 0 0)))))))

(defun bmp-parse-version (version-str)
  (-map #'string-to-number (s-split "\\." version-str)))

(defun bmp-unparse-version (version)
  (s-join "." (-map #'number-to-string version)))

(defun bmp-commit (files version-str)
  "Simple blocking git add and commit. Should use magit here someday."
  (let ((args (s-join " " (-map #'shell-quote-argument files))))
    (shell-command-to-string (format "git add %s" args))
    (shell-command-to-string (format "git commit -m \"%s\"" version-str))))

(defun bmp-tag (version-str)
  (magit-tag version-str "master"))

(provide 'bmp)

;;; bmp.el ends here
