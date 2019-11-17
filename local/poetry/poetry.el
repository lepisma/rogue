;;; poetry.el --- poetry tools -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))

;;; Commentary:

;; poetry tools
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

(require 'pyvenv)
(require 'f)
(require 'helm)

;;;###autoload
(defun poetry-activate ()
  "Activate a poetry virtual environment."
  (interactive)
  (let* ((venv-dir "~/.cache/pypoetry/virtualenvs/")
         (envs (directory-files venv-dir nil "^[a-z]")))
    (helm :sources (helm-build-sync-source "virtualenvs"
                     :candidates envs
                     :action `(("Activate venv" . (lambda (env) (pyvenv-activate (f-join (f-expand ,venv-dir) env))))))
          :buffer "*helm poetry*"
          :prompt "Activate : ")))

;;;###autoload
(defun poetry-deactivate ()
  (interactive)
  (pyvenv-deactivate))

(defun poetry-ensure-activation ()
  (unless pyvenv-virtual-env
    (error "No virtualenv activate.")))

;;;###autoload
(defun poetry-link-global ()
  "Pull in global package from site-packages in current
virtualenv."
  (interactive)
  (poetry-ensure-activation))

;;;###autoload
(defun poetry-unlink-global ()
  "Unlink a previously pulled package from global environment."
  (interactive)
  (poetry-ensure-activation))

(provide 'poetry)

;;; poetry.el ends here
