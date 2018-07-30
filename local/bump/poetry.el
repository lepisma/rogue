;;; poetry.el --- Utilities for working with poetry  -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/lepisma/poetry.el

;;; Commentary:

;; Utilities for working with poetry
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
(require 'dash-functional)

(defun poetry-bump-cmd (type)
  (shell-command-to-string (format "poetry version %s" type)))

(defun poetry-get-meta (key)
  (with-current-buffer (find-file-noselect (f-join default-directory "pyproject.toml"))
    (goto-char (point-min))
    (re-search-forward (format "^%s ?= ?\"?\\(.*?\\)\"?$" key))
    (match-string-no-properties 1)))

(defun poetry-bump-init (version)
  (let ((name (poetry-get-meta "name")))
    (with-current-buffer (find-file-noselect (f-join default-directory name "__init__.py"))
      (goto-char (point-min))
      (re-search-forward "^__version__ = \"?\\(.*?\\)\"?$")
      (replace-match version nil nil nil 1)
      (save-buffer))))

(defun poetry-bump-test (version)
  (let ((name (poetry-get-meta "name")))
    (with-current-buffer (find-file-noselect (f-join default-directory "tests" (format "test_%s.py" name)))
      (goto-char (point-min))
      (re-search-forward "assert __version__ == \"?\\(.*?\\)\"?$")
      (replace-match version nil nil nil 1)
      (save-buffer))))

(defun poetry-git-stage ()
  (let* ((name (poetry-get-meta "name"))
         (files (-map (-cut f-join default-directory <>)
                      (list "pyproject.toml" (f-join name "__init__.py") (f-join "tests" (format "test_%s.py" name))))))
    (-each files (-cut magit-stage-file <>))))

(defun poetry-git-tag ()
  (let ((version (poetry-get-meta "version")))
    (magit-commit `("-m" ,version))
    (sleep-for 1) ;; Lol
    (magit-tag version "master")))

(defun poetry-bump ()
  (interactive)
  (poetry-bump-cmd "patch")
  (let ((version (poetry-get-meta "version")))
    (poetry-bump-init version)
    (poetry-bump-test version)
    (poetry-git-stage)
    (poetry-git-tag)
    (message "Bumped to version %s" version)))

(provide 'poetry)

;;; poetry.el ends here
