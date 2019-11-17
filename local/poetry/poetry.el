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
(require 'cl-lib)

(defcustom poetry-venv-root "~/.cache/pypoetry/virtualenvs/"
  "Path to the virtual environment directory for poetry.")

(defcustom poetry-global-site-packages-dir "~/.pyenv/versions/3.6.8/lib/python3.6/site-packages/"
  "Path to global site packages directory.")

;;;###autoload
(defun poetry-activate ()
  "Activate a poetry virtual environment."
  (interactive)
  (let* ((envs (directory-files poetry-venv-root nil "^[a-z]")))
    (helm :sources (helm-build-sync-source "virtualenvs"
                     :candidates envs
                     :action `(("Activate venv" . (lambda (env) (pyvenv-activate (f-join (f-expand ,poetry-venv-root) env))))))
          :buffer "*helm poetry*"
          :prompt "Activate : ")))

;;;###autoload
(defun poetry-deactivate ()
  (interactive)
  (pyvenv-deactivate))

(defun poetry-venv-python-version (venv-dir)
  "Return python version used in VENV-DIR. This relies on
poetry's naming conventions."
  (string-match "py\\([0-9]\.[0-9]\\)/?$" venv-dir)
  (match-string-no-properties 1 venv-dir))

(defun poetry-venv-site-packages-dir (venv-dir)
  "Return path to site packages for given poetry VENV-DIR."
  (let ((python-version (poetry-venv-python-version venv-dir)))
    (concat (file-name-as-directory venv-dir) "lib/python" python-version "/site-packages/")))

(defun poetry-ensure-activation ()
  (unless pyvenv-virtual-env
    (error "No virtualenv activate.")))

(defun poetry-list-packages (site-packages-dir)
  "List package names, along with properties, available in given
SITE-PACKAGES-DIR."
  ;; TODO: Have better return items from here. At present, we just returns
  ;;       directory entries, putting the burden of identifying packages on the
  ;;       user.
  (mapcar (lambda (entry) (cons (f-filename entry) entry)) (directory-files site-packages-dir t "^[a-zA-Z_-]")))

(defun poetry-list-symlink-packages (site-packages-dir)
  "List packages symlinked in SITE-PACKAGES-DIR."
  (cl-remove-if-not (lambda (it) (f-symlink? (cdr it))) (poetry-list-packages site-packages-dir)))

;;;###autoload
(defun poetry-link-global ()
  "Pull in global package from site-packages in current
virtualenv."
  (interactive)
  (poetry-ensure-activation)
  (let* ((items (poetry-list-packages poetry-global-site-packages-dir))
         (picked (helm :sources (helm-build-sync-source "Global items"
                                  :candidates items)))
         (link-path (f-join (poetry-venv-site-packages-dir pyvenv-virtual-env) (f-filename picked))))
    (if (f-exists? link-path)
        (error "Link path %s already exists" link-path)
      (make-symbolic-link picked link-path)
      (message "Linking at %s" link-path))))

;;;###autoload
(defun poetry-unlink-global ()
  "Unlink a previously pulled package from global environment."
  (interactive)
  (poetry-ensure-activation))

(provide 'poetry)

;;; poetry.el ends here
