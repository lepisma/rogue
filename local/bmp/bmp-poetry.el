;;; bmp-poetry.el --- Poetry support for bmp -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Poetry support for bmp
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
(require 'bmp-project)
(require 'eieio)

(defclass bmp-poetry-project (bmp-project)
  ((name :initarg :name)
   (toml-file :initarg :toml-file)
   (test-file :initarg :test-file)
   (init-file :initarg :init-file))
  "A poetry project")

(defun bmp-poetry-get-project ()
  (let ((toml-file "pyproject.toml"))
    (if (f-exists? (f-join default-directory toml-file))
        (let ((name (bmp-poetry-get-meta (f-join default-directory toml-file) "name")))
          (bmp-poetry-project
           :root-dir default-directory
           :name name
           :toml-file toml-file
           :test-file (format "tests/test_%s.py" name)
           :init-file (format "%s/__init__.py" name))))))

(cl-defmethod bmp-get-version ((obj bmp-poetry-project))
  (let ((toml-path (f-join (oref obj :root-dir) (oref obj :toml-file))))
    (bmp-poetry-get-meta toml-path "version")))

(cl-defmethod bmp-set-version ((obj bmp-poetry-project) version-str)
  (let ((default-directory (oref obj :root-dir)))
    (shell-command-to-string (format "poetry version %s" version-str))
    (bmp-poetry-set-test (oref obj :test-file) version-str)
    (bmp-poetry-set-init (oref obj :init-file) version-str)))

(cl-defmethod bmp-get-files ((obj bmp-poetry-project))
  (list (oref obj :toml-file)
        (oref obj :test-file)
        (oref obj :init-file)))

(defun bmp-poetry-set-init (init-path version-str)
  (with-current-buffer (find-file-noselect init-path)
    (goto-char (point-min))
    (re-search-forward "^__version__ = \"?\\(.*?\\)\"?$")
    (replace-match version-str nil nil nil 1)
    (save-buffer)))

(defun bmp-poetry-set-test (test-path version-str)
  (with-current-buffer (find-file-noselect test-path)
    (goto-char (point-min))
    (re-search-forward "assert __version__ == \"?\\(.*?\\)\"?$")
    (replace-match version-str nil nil nil 1)
    (save-buffer)))

(defun bmp-poetry-get-meta (toml-path key)
  "Parse values from toml"
  (with-current-buffer (find-file-noselect toml-path)
    (goto-char (point-min))
    (re-search-forward (format "^%s ?= ?\"?\\(.*?\\)\"?$" key))
    (match-string-no-properties 1)))

(provide 'bmp-poetry)

;;; bmp-poetry.el ends here
