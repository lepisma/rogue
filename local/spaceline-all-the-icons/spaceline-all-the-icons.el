;;; spaceline-all-the-icons.el --- Custom install for all the icons Spaceline

;; Copyright (C) 2016  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'spaceline)
(require 'spaceline-config)
(require 'all-the-icons)

;;---------------;;
;; First Segment ;;
;;---------------;;

(spaceline-define-segment
    ati-modified "An `all-the-icons' modified segment"
    (let* ((config-alist
            '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1.2 :v-adjust -0.0)
              ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1.2 :v-adjust -0.0)
              ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.2 :v-adjust 0.1)))
           (result (cdr (assoc (format-mode-line "%*") config-alist))))

      (propertize (format "%s" (apply (cadr result) (cddr result))) 'face `(:family ,(funcall (car result)) :inherit )))
    :tight t)

(spaceline-define-segment
    ati-window-numbering "An `all-the-icons' window numbering segment"
    (propertize (format "%c" (+ 9311 (window-numbering-get-number)))
                'face `(:height 1.3 :inherit)
                'display '(raise -0.0))
    :tight t :when (fboundp 'window-numbering-mode))

(spaceline-define-segment
    ati-projectile "An `all-the-icons' segment for current `projectile' project"
    (concat
     (propertize "|" 'face '(:height 1.1 :inherit))
     " "
     (if (and (fboundp 'projectile-project-name)
              (projectile-project-name))
         (propertize (format "%s" (concat (projectile-project-name) ))
                     'face '(:height 0.8 :inherit)
                     'display '(raise 0.2)
                     'help-echo "Switch Project"
                     'mouse-face '(:box 1)
                     'local-map (make-mode-line-mouse-map
                                 'mouse-1 (lambda () (interactive) (projectile-switch-project))))
       (propertize "×" 'face '(:height 0.8 :inherit)))
     " "
     (propertize "|" 'face '(:height 1.1 :inherit)))
    :tight t)

(spaceline-define-segment
    ati-mode-icon "An `all-the-icons' segment for the current buffer mode"
    (let ((icon (all-the-icons-icon-for-buffer)))
      (unless (symbolp icon) ;; This implies it's the major mode
        (propertize icon
                    'help-echo (format "Major-mode: `%s`" major-mode)
                    'display '(raise 0.0)
                    'face `(:height 1.0 :family ,(all-the-icons-icon-family-for-buffer) :inherit)))))

(spaceline-define-segment
    ati-buffer-id "An `all-the-icons' segment for the current buffer id"
    (if (fboundp 'projectile-project-root)
        (let* ((buf (or (buffer-file-name) (buffer-name)))
               (proj (ignore-errors (projectile-project-root)) )
               (name (if (buffer-file-name)
                         (or (cadr (split-string buf proj))
                             (format-mode-line "%b"))
                       (format-mode-line "%b"))))
          (propertize (format "%s" name)
                      'face `(:height 0.8 :inherit)
                      'display '(raise 0.2)
                      'help-echo (format "Major-mode: `%s`" major-mode)))
      (propertize (format-mode-line "%b ") 'face '(:height 0.8 :inherit) 'display '(raise 0.1)))
    :tight t)

;;----------------;;
;; Second Segment ;;
;;----------------;;

(spaceline-define-segment
    ati-process "An `all-the-icons' segment for the current process"
    (let ((icon (all-the-icons-icon-for-buffer)))
      (concat
       (when (or (symbolp icon) mode-line-process)
         (propertize (format-mode-line "%m") 'face `(:height 0.8 :inherit) 'display '(raise 0.2)))
       (when mode-line-process
         (propertize (format-mode-line mode-line-process) 'face '(:height 0.7 :inherit) 'display '(raise 0.2)))))
    :tight t)

(spaceline-define-segment
    ati-position "An `all-the-icons' segment for the Row and Column of the current point"
    (propertize (format-mode-line "%l:%c") 'face `(:height 0.9 :inherit) 'display '(raise 0.1)))

(spaceline-define-segment
    ati-region-info "An `all-the-icons' segment for the currently marked region"
    (when mark-active
      (let ((words (count-lines (region-beginning) (region-end)))
            (chars (count-words (region-end) (region-beginning))))
        (concat
         (propertize (format "%s " (all-the-icons-octicon "pencil") words chars)
                     'face `(:family ,(all-the-icons-octicon-family) :inherit) 'display '(raise 0.1))
         (propertize (format "(%s, %s)" words chars)
                     'face `(:height 0.9 :inherit))))))

(spaceline-define-segment
    ati-color-control "An `all-the-icons' segment for the currently marked region" "")


;;----------------;;
;; Third Segement ;;
;;----------------;;

(defun spaceline---github-vc ()
  "Function to return the Spaceline formatted GIT Version Control text."
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (all-the-icons-alltheicon "git") 'face '(:height 1.1 :inherit) 'display '(raise 0.1))
     (propertize " · ")
     (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                 'face `(:family ,(all-the-icons-octicon-family) :height 1.0 :inherit)
                 'display '(raise 0.2))
     (propertize (format " %s" branch) 'face `(:height 0.9 :inherit) 'display '(raise 0.2)))))

(defun spaceline---svn-vc ()
  "Function to return the Spaceline formatted SVN Version Control text."
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
     (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.2) 'display '(raise -0.1))
     (propertize (format " · %s" revision) 'face `(:height 0.9)))))


(spaceline-define-segment
    ati-vc-icon "An `all-the-icons' segment for the current Version Control icon"
    (when vc-mode
      (cond ((string-match "Git[:-]" vc-mode) (spaceline---github-vc))
            ((string-match "SVN-" vc-mode) (spaceline---svn-vc))
            (t (propertize (format "%s" vc-mode)))))
    :when active)

(spaceline-define-segment
    ati-flycheck-status "An `all-the-icons' representaiton of `flycheck-status'"
    (let* ((text
            (pcase flycheck-last-status-change
              (`finished (if flycheck-current-errors
                             (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                            (+ (or .warning 0) (or .error 0)))))
                               (format "✖ %s Issue%s" count (if (eq 1 count) "" "s")))
                           "✔ No Issues"))
              (`running     "⟲ Running")
              (`no-checker  "⚠ No Checker")
              (`not-checked "✖ Disabled")
              (`errored     "⚠ Error")
              (`interrupted "⛔ Interrupted")
              (`suspicious  "")))
           (f (cond
               ((string-match "⚠" text) `(:height 0.9 :foreground ,(face-attribute 'spaceline-flycheck-warning :foreground)))
               ((string-match "✖ [0-9]" text) `(:height 0.9 :foreground ,(face-attribute 'spaceline-flycheck-error :foreground)))
               ((string-match "✖ Disabled" text) `(:height 0.9 :foreground ,(face-attribute 'font-lock-comment-face :foreground)))
               (t '(:height 0.9 :inherit)))))
      (propertize (format "%s" text)
                  'face f
                  'help-echo "Show Flycheck Errors"
                  'display '(raise 0.2)
                  'mouse-face '(:box 1)
                  'local-map (make-mode-line-mouse-map 'mouse-1 (lambda () (interactive) (flycheck-list-errors)))))
    :when active :tight t )

(defvar spaceline--upgrades nil)
(defun spaceline--count-upgrades ()
  "Function to count the number of package upgrades needed."
  (let ((buf (current-buffer)))
    (package-list-packages-no-fetch)
    (with-current-buffer "*Packages*"
      (setq spaceline--upgrades (length (package-menu--find-upgrades))))
    (switch-to-buffer buf)))
(advice-add 'package-menu-execute :after 'spaceline--count-upgrades)

(spaceline-define-segment
    ati-package-updates "An `all-the-icons' spaceline segment to indicate number of package updates needed"
    (let ((num (or spaceline--upgrades (spaceline--count-upgrades))))
      (propertize
       (concat
        (propertize (format "%s" (all-the-icons-octicon "package"))
                    'face `(:family ,(all-the-icons-octicon-family) :height 1.1 :inherit)
                    'display '(raise 0.1))
        (propertize (format " %d updates " num) 'face `(:height 0.9 :inherit) 'display '(raise 0.2)))
       'help-echo "Open Packages Menu"
       'mouse-face '(:box 1)
       'local-map (make-mode-line-mouse-map
                   'mouse-1 (lambda () (interactive) (package-list-packages)))))
    :when (and active (> (or spaceline--upgrades (spaceline--count-upgrades)) 0)))

;;---------------------;;
;; Right First Segment ;;
;;---------------------;;
(defun spaceline--get-temp ()
  "Function to return the Temperature formatted for ATI Spacline."
  (let ((temp (yahoo-weather-info-format yahoo-weather-info "%(temperature)")))
    (unless (string= "" temp) (format "%s°C" (round (string-to-number temp))))))

(spaceline-define-segment
    ati-weather "Weather"
    (let* ((weather (yahoo-weather-info-format yahoo-weather-info "%(weather)"))
           (temp (spaceline--get-temp))
           (help (concat "Weather is '" weather "' and the temperature is " temp))
           (icon (all-the-icons-icon-for-weather (downcase weather))))
      (concat
       (if (> (length icon) 1)
           (propertize icon 'help-echo help 'face `(:height 0.9 :inherit) 'display '(raise 0.1))
           (propertize icon
                    'help-echo help
                    'face `(:height 0.9 :family ,(all-the-icons-wicon-family) :inherit)
                    'display '(raise 0.0)))
       (propertize " " 'help-echo help)
       (propertize (spaceline--get-temp) 'face '(:height 0.9 :inherit) 'help-echo help)))
    :when (and active (boundp 'yahoo-weather-info) yahoo-weather-mode)
    :enabled nil
    :tight t)

(spaceline-define-segment
    ati-suntime "Suntime"
    (let ((help (yahoo-weather-info-format yahoo-weather-info "Sunrise at %(sunrise-time), Sunset at %(sunset-time)")))
      (concat
       (propertize (yahoo-weather-info-format yahoo-weather-info "%(sunrise-time) ")
                   'face '(:height 0.9 :inherit) 'display '(raise 0.1) 'help-echo help)
       (propertize (format "%s" (all-the-icons-wicon "sunrise" :v-adjust 0.1))
                   'face `(:height 0.8 :family ,(all-the-icons-wicon-family) :inherit) 'help-echo help)
       (propertize " · " 'help-echo help)
       (propertize (yahoo-weather-info-format yahoo-weather-info "%(sunset-time) ")
                   'face '(:height 0.9 :inherit) 'display '(raise 0.1) 'help-echo help)
       (propertize (format "%s" (all-the-icons-wicon "sunset" :v-adjust 0.1))
                   'face `(:height 0.8 :family ,(all-the-icons-wicon-family) :inherit) 'help-echo help)))
    :when (and active (boundp 'yahoo-weather-info) yahoo-weather-mode)
    :enabled nil
    :tight t )

(spaceline-define-segment
    ati-time "Time"
    (let* ((hour (string-to-number (format-time-string "%I")))
           (icon (all-the-icons-wicon (format "time-%s" hour) :v-adjust 0.0)))
      (concat
       (propertize (format-time-string "%H:%M ") 'face `(:height 0.9 :inherit) 'display '(raise 0.1))
       (propertize (format "%s" icon)
                   'face `(:height 0.8 :family ,(all-the-icons-wicon-family) :inherit)
                   'display '(raise 0.1))))
    :tight t)

(spaceline-define-segment
    ati-height-modifier "Modifies the height of inactive buffers"
    (propertize " " 'face '(:height 1.3 :inherit))
    :tight t :when (not active))

(spaceline-define-segment
    ati-buffer-size "Buffer Size"
    (propertize (format-mode-line "%I") 'face `(:height 0.9 :inherit) 'display '(raise 0.1))
    :tight t)

(spaceline-define-segment
    ati-battery-status "Show battery information"
    (let* ((charging? (equal "AC" (cdr (assoc ?L fancy-battery-last-status))))
           (percentage (string-to-int (cdr (assoc ?p fancy-battery-last-status))))
           (time (format "%s" (cdr (assoc ?t fancy-battery-last-status))))
           (icon-set (if charging? 'alltheicon 'faicon))
           (icon-alist
            (cond
             (charging? '((icon . "charging") (inherit . success) (height . 1.3) (raise . -0.1)))
             ((> percentage 95) '((icon . "full") (inherit . success)))
             ((> percentage 70) '((icon . "three-quarters")))
             ((> percentage 35) '((icon . "half")))
             ((> percentage 15) '((icon . "quarter") (inherit . warning)))
             (t '((icon . "empty") (inherit . error)))))
           (icon-f (all-the-icons--function-name icon-set))
           (family (funcall (all-the-icons--family-name icon-set))))
      (let-alist icon-alist
        (concat
         (if .inherit
             (let ((fg (face-attribute .inherit :foreground)))
               (propertize (funcall icon-f (format "battery-%s" .icon))
                           'face `(:height ,(or .height 1.0) :family ,family :foreground ,fg)
                           'display `(raise ,(or .raise 0.0))))
             (propertize (funcall icon-f (format "battery-%s" .icon))
                         'face `(:family ,family :inherit)
                         'display '(raise 0.0)))
         " "
         (if .inherit
             (let ((fg (face-attribute .inherit :foreground)))
               (propertize (if charging? (format "%s%%%%" percentage) time) 'face `(:height 0.9 :foreground ,fg)))
           (propertize time 'face '(:height 0.9 :inherit)))
         )))
    :global-override fancy-battery-mode-line :when (and active (fboundp 'fancy-battery-mode) fancy-battery-mode))

(defun spaceline--direction (dir)
  "Inverts DIR from right to left & vice versa."
  (if spaceline-invert-direction (if (equal dir "right") "left" "right") dir))

(defun spaceline--separator-type ()
  "Static function to return the separator type."
  spaceline-separator-type)

(defmacro define-separator (name dir start-face end-face &optional invert)
  "Macro to defined a NAME separator in DIR direction.
Provide the START-FACE and END-FACE to describe the way it should
fade between segmeents.  When INVERT is non-nil, it will invert
the directions of the separator."
  `(progn
     (spaceline-define-segment
         ,(intern (format "ati-%s-separator" name))
       (let ((dir (if spaceline-invert-direction (spaceline--direction ,dir) ,dir))
             (sep (spaceline--separator-type)))
         (propertize (all-the-icons-alltheicon (format "%s-%s" sep dir) :v-adjust 0.0)
                     'face `(:height 1.5
                             :family
                             ,(all-the-icons-alltheicon-family)
                             :foreground
                             ,(face-attribute ,start-face :background)
                             :background
                             ,(face-attribute ,end-face :background))))
       :skip-alternate t :tight t :when (if ,invert (not active) active))))

(defvar spaceline-invert-direction t)
(defvar spaceline-separator-type "slant")

(define-separator "left-inactive" "right" 'powerline-inactive1 'powerline-inactive2 t)
(define-separator "right-inactive" "left" 'powerline-inactive2 'mode-line-inactive t)

(define-separator "left-1" "right" 'spaceline-highlight-face 'powerline-active1)
(define-separator "left-2" "right" 'powerline-active1 'spaceline-highlight-face)
(define-separator "left-3" "right" 'spaceline-highlight-face 'mode-line)
(define-separator "left-4" "right" 'mode-line 'powerline-active2)

(define-separator "right-1" "left" 'powerline-active2 'powerline-active1)
(define-separator "right-2" "left" 'powerline-active1 'mode-line)

(spaceline-compile
 "ati"
 '(
   ((ati-modified ati-window-numbering ati-buffer-size) :face highlight-face :skip-alternate t)
   ;; left-active-3
   ati-left-1-separator
   ((ati-projectile ati-mode-icon ati-buffer-id) :face default-face)
   ati-left-2-separator
   ((ati-process ati-position ati-region-info) :face highlight-face :separator " | ")
   ati-left-3-separator
   ati-left-inactive-separator
   ((ati-vc-icon ati-flycheck-status ati-package-updates purpose) :separator " · " :face other-face)
   ati-left-4-separator)

 '(ati-right-1-separator
   ((ati-suntime ati-weather) :separator " · " :face other-face)
   ati-right-2-separator
   ati-right-inactive-separator
   ((ati-battery-status ati-time) :separator " | " :face other-face)
   ))

;; (setq mode-line-format '("%e" (:eval (spaceline-ml-main))))

(provide 'spaceline-all-the-icons)
;;; spaceline-all-the-icons.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
