;;; rogue-utils.el --- Utility functions for rogue

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/lepisma/rogue/tree/master/local/rogue-utils

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

;;;###autoload
(defun rogue-utils-add-hooks (hooks fun)
  "Add FUN to all the HOOKS."
  (dolist (hook hooks)
    (add-hook hook fun)))

(provide 'rogue-utils)

;;; rogue-utils.el ends here
