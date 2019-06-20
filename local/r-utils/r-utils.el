;;; r-utils.el --- Utility functions for rogue

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Personal config package with utilities
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

(require 'dash-functional)

;;;###autoload
(defun r-utils/add-hooks (hooks fns)
  "Add FUNs to all the HOOKS. Works multiway."
  (dolist (hook hooks)
    (dolist (fn fns)
      (add-hook hook fn t))))

;;;###autoload
(defun r-utils/get-project-dirs (names)
  "Return full paths to given project NAMES. Relies on a variable
user-project-dir."
  (mapcar (-cut concat user-project-dir <>) names))

(provide 'r-utils)

;;; r-utils.el ends here
