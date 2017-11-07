;;; viz.el --- Some color based visualization functions

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25") (colormaps "0.1.2"))
;; URL: https://github.com/lepisma/rogue/tree/master/local/viz

;;; Commentary:

;; viz.el provides some (mostly color based) visualization functions for Emacs
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
(require 'colormaps)

(defun viz-color-buffer-text (text color)
  "Color the text in current buffer."
  (goto-char (point-min))
  (while (search-forward text nil t)
    (put-text-property (- (point) (length text)) (point)
                       'font-lock-face `(:background ,color))))

(defun viz-get-buffer-numbers ()
  "Get a list of all numbers in current buffer."
  (let ((text (s-collapse-whitespace (substring-no-properties (buffer-string)))))
    (cl-remove-if #'zerop (mapcar #'string-to-number (s-split "," (s-replace " " "," text))))))

(defun viz--get-csv-row-best (line &optional best-func)
  "Return best value from the csv row. BEST-FUNC finds the best from a list of numbers. Defaults to
min."
  (let* ((tokens (s-split "," (s-collapse-whitespace line)))
         (items (mapcar #'string-to-number (cl-remove-if (lambda (x) (= 0 (string-to-number x))) tokens))))
    (if items
        (number-to-string (apply (or best-func #'min) items))
      nil)))

;;;###autoload
(defun viz-unhighlight-csv ()
  "Unhighlight all from the csv."
  (interactive)
  (unhighlight-regexp t))

;;;###autoload
(defun viz-csv-row-best (&optional best-func highlight-face)
  "Highlight best row in the csv. BEST-FUNC finds the best from a list of numbers. HIGHLIGHT-FACE
defines the face to use for highlighting."
  (interactive)
  (viz-unhighlight-csv)
  (let ((lines (s-split "\n" (buffer-string))))
    (mapc (lambda (line)
            (let ((best-val (viz--get-csv-row-best line best-func)))
              (if best-val
                  (highlight-regexp best-val (or highlight-face 'hi-yellow))))) lines)))

;;;###autoload
(defun viz-csv-heat-map (&optional cmap)
  (interactive)
  (viz-unhighlight-csv)
  (let* ((nums (viz-get-buffer-numbers))
         (nums-min (float (apply #'min nums)))
         (nums-max (float (apply #'max nums))))
    (mapc (lambda (n) (let ((value (/ (- n nums-min) (- nums-max nums-min))))
                   (viz-color-buffer-text (number-to-string n) (colormaps-get-color value cmap)))) nums)))

(provide 'viz)
;;; viz.el ends here
