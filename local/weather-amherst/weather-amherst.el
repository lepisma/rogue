;;; weather-amherst.el --- Weather for Amherst from UMassCS Station

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25") (enlive "0.0.1"))
;; URL: https://github.com/lepisma/rogue/tree/master/local/weather-amherst

;;; Commentary:

;; weather-amherst displays current weather information from the station at
;; UMassCS, Amherst
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

(require 'enlive)
(require 'org)

(defun weather-amherst-transform-units (pairs)
  "Transform unit PAIRS to SI. Just temp for now."
  (mapcar
   (lambda (pair)
     (let* ((split (split-string (second pair) "°"))
            (value (string-to-number (first split)))
            (unit (second split)))
       (if (string-equal unit "F")
           `(,(first pair)
             ,(concat (format "%0.2f" (/ (- value 32) 1.8)) "°C"))
         pair))) pairs))

(defun weather-amherst-show-in-buffer (pairs location)
  "Display weather data in a new buffer."
  (let ((buffer (get-buffer-create "*Weather*")))
    (set-buffer buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (org-mode)
    (insert "#+TITLE: ")
    (insert location)
    (insert "\n\n")
    (mapc (lambda (pair) (insert (concat "+ " (first pair) " :: " (second pair) "\n")))
          (weather-amherst-transform-units (butlast pairs)))
    (switch-to-buffer buffer)
    (setq buffer-read-only t)
    (goto-char (point-min))))

;;;###autoload
(defun weather-amherst ()
  "Get local weather information for Amherst from CS station."
  (interactive)
  (let* ((rss-url "http://weather.cs.umass.edu/RSS/weewx_rss.xml")
         (location "Amherst, MA (USA)")
         (node (first (enlive-get-elements-by-tag-name
                       (enlive-fetch rss-url) 'encoded)))
         (items (split-string (enlive-text node) "\n" t)))
    (weather-amherst-show-in-buffer
     (mapcar (lambda (item)
               (mapcar 'string-trim (split-string item ": "))) items) location)
    (weather-amherst-mode)))

(defvar weather-amherst-mode-map (make-sparse-keymap))

(define-key weather-amherst-mode-map (kbd "q") 'kill-this-buffer)

(define-minor-mode weather-amherst-mode
  "Minor mode for adding keybindings"
  nil nil weather-amherst-mode-map)

(provide 'weather-amherst)
;;; weather-amherst.el ends here
