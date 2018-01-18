;;; spaceline-all-the-icons.el --- Custom install for all the icons Spaceline

;; Copyright (C) 2016  Dominic Charlesworth <dgc336@gmail.com>
;; Copyright (C) 2017  Abhinav Tushar <abhinav.tushar.vs@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;;         Abhinav Tushar <abhinav.tushar.vs@gmail.com>
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

(require 'all-the-icons)
(require 's)
(require 'spaceline)
(require 'spaceline-config)

(spaceline-define-segment ati-persp-name
  "The current perspective name."
  (when (and active
             (bound-and-true-p persp-mode)
             ;; There are multiple implementations of
             ;; persp-mode with different APIs
             (fboundp 'safe-persp-name)
             (fboundp 'get-frame-persp))
    (let ((name (safe-persp-name (get-frame-persp))))
      (propertize
       (downcase (if (file-directory-p name)
                     (file-name-nondirectory (directory-file-name name))
                   name))
       'face `(:height 0.8
                       :background ,(face-attribute 'default :background)
                       :foreground ,(face-attribute 'font-lock-doc-face :foreground))
       'display '(raise 0.2)))))

(spaceline-define-segment
    ati-modified "An `all-the-icons' modified segment"
    (let* ((config-alist
            '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :v-adjust 0.2)
              ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :v-adjust 0.2)
              ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :v-adjust 0.2)))
           (result (cdr (assoc (format-mode-line "%*") config-alist))))

      (propertize (format "%s" (apply (cadr result) (cddr result))) 'face `(:family ,(funcall (car result)) :height 0.9 :inherit )))
    :tight t)

(spaceline-define-segment
    ati-buffer-size "Buffer Size"
    (propertize (format-mode-line "%I") 'face `(:height 0.8 :inherit) 'display '(raise 0.2))
    :tight t)

(spaceline-define-segment
    ati-projectile "An `all-the-icons' segment for current `projectile' project"
    (concat
     (if (and (fboundp 'projectile-project-name)
              (projectile-project-name))
         (propertize (format "%s" (projectile-project-name))
                     'face '(:height 0.8 :inherit)
                     'display '(raise 0.2))
       (propertize "×" 'face '(:height 0.8 :inherit)))
     " "
     (propertize "|" 'face '(:height 0.8 :inherit) 'display '(raise 0.2)))
    :tight t)

(spaceline-define-segment
    ati-mode-icon "An `all-the-icons' segment for the current buffer mode"
    (let ((icon (all-the-icons-icon-for-buffer)))
      (unless (symbolp icon) ;; This implies it's the major mode
        (propertize icon
                    'help-echo (format "Major-mode: `%s`" major-mode)
                    'display '(raise 0.2)
                    'face `(:height 0.7 :family ,(all-the-icons-icon-family-for-buffer) :inherit)))))

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
      (propertize (format-mode-line "%b ") 'face '(:height 0.8 :inherit) 'display '(raise 0.2)))
    :tight t)

(spaceline-define-segment
    ati-position "An `all-the-icons' segment for the Row and Column of the current point"
    (propertize (format-mode-line "%l:%c") 'face `(:height 0.8 :inherit) 'display '(raise 0.2)))

(spaceline-define-segment
    ati-region-info "An `all-the-icons' segment for the currently marked region"
    (when mark-active
      (let ((words (count-lines (region-beginning) (region-end)))
            (chars (count-words (region-end) (region-beginning))))
        (propertize (format "(%s, %s)" words chars)
                    'face `(:height 0.8 :inherit)
                    'display '(raise 0.2)))))

(defun spaceline---github-vc ()
  "Function to return the Spaceline formatted GIT Version Control text."
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                 'face `(:family ,(all-the-icons-octicon-family) :height 0.9 :inherit)
                 'display '(raise 0.3))
     (propertize (format " %s" branch) 'face `(:height 0.8 :inherit) 'display '(raise 0.3)))))

(spaceline-define-segment
    ati-vc-icon "An `all-the-icons' segment for the current Version Control icon"
    (when vc-mode
      (cond ((string-match "Git[:-]" vc-mode) (spaceline---github-vc))
            (t (propertize (format "%s" vc-mode)))))
    :when active)

(spaceline-define-segment
    ati-flycheck-status "An `all-the-icons' representaiton of `flycheck-status'"
    (let* ((text
            (pcase flycheck-last-status-change
              (`finished (if flycheck-current-errors
                             (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                            (+ (or .warning 0) (or .error 0)))))
                               (format "✖ %s issue%s" count (if (eq 1 count) "" "s")))
                           "✔ no issues"))
              (`running     "↻ running")
              (`no-checker  "! no checker")
              (`not-checked "disabled")
              (`errored     "! error")
              (`interrupted "! interrupted")
              (`suspicious  "")))
           (f (cond
               ((string-match "! " text) `(:height 0.8 :foreground ,(face-attribute 'spaceline-flycheck-warning :background)
                                                   :background ,(face-attribute 'highlight :background)))
               ((string-match "✖ [0-9]" text) `(:height 0.8 :foreground "#ff6347"
                                                        :background ,(face-attribute 'highlight :background)))
               ((string-match "disabled" text) `(:height 0.8 :foreground ,(face-attribute 'font-lock-comment-face :foreground)
                                                         :background ,(face-attribute 'highlight :background)))
               (t '(:height 0.8 :inherit)))))
      (propertize (format "%s" text)
                  'face f
                  'help-echo "Show Flycheck Errors"
                  'display '(raise 0.3)
                  'local-map (make-mode-line-mouse-map 'mouse-1 (lambda () (interactive) (flycheck-list-errors)))))
    :when active :tight t )

(spaceline-define-segment
    ati-time "Time"
    (let* ((hour (string-to-number (format-time-string "%I")))
           (icon (all-the-icons-wicon (format "time-%s" hour) :v-adjust 0.2)))
      (concat
       (propertize (format "%s  " icon)
                   'face `(:height 0.8 :family ,(all-the-icons-wicon-family) :inherit)
                   'display '(raise 0.2))
       (propertize (format-time-string "%H:%M") 'face `(:height 0.8 :inherit) 'display '(raise 0.3))))
    :tight t)

(spaceline-define-segment
    ati-height-modifier "Modifies the height of inactive buffers"
    (propertize " " 'face '(:height 1.8 :inherit))
    :tight t :when (not active))

(spaceline-define-segment ati-buffer-position
  "The current approximate buffer position, in percent."
  (propertize (format-mode-line "%p ")
              'face '(:height 0.8 :inherit) 'display '(raise 0.3)))

(defvar spaceline-org-clock-format-function
  'org-clock-get-clock-string
  "The function called by the `org-clock' segment to determine what to show.")

(spaceline-define-segment ati-org-clock
  "Show information about the current org clock task.  Configure
`spaceline-org-clock-format-function' to configure. Requires a currently running
org clock.
This segment overrides the modeline functionality of `org-mode-line-string'."
  (when (and (fboundp 'org-clocking-p)
             (org-clocking-p))
    (concat
     (propertize (all-the-icons-faicon "hourglass-half")
                 'face `(:family ,(all-the-icons-faicon-family) :height 0.7 :inherit)
                 'display '(raise 0.4))
     " "
     (propertize (s-truncate
                  30 (substring-no-properties (funcall spaceline-org-clock-format-function)))
                 'face '(:height 0.8 :inherit ) 'display '(raise 0.3))))
  :global-override org-mode-line-string)

(spaceline-define-segment ati-org-pomodoro
  "Shows the current pomodoro.  Requires `org-pomodoro' to be active.
This segment overrides the modeline functionality of `org-pomodoro' itself."
  (when (and (fboundp 'org-pomodoro-active-p)
             (org-pomodoro-active-p))
    (propertize (nth 1 org-pomodoro-mode-line)
                'face '(:height 0.8 :inherit ) 'display '(raise 0.3)))
  :global-override org-pomodoro-mode-line)

(defun spaceline--direction (dir)
  "Inverts DIR from right to left & vice versa."
  (if spaceline-invert-direction (if (equal dir "right") "left" "right") dir))

(defmacro define-separator (name dir start-face end-face &optional invert)
  "Macro to defined a NAME separator in DIR direction.
Provide the START-FACE and END-FACE to describe the way it should
fade between segmeents.  When INVERT is non-nil, it will invert
the directions of the separator."
  `(progn
     (spaceline-define-segment
         ,(intern (format "ati-%s-separator" name))
       (let ((dir (if spaceline-invert-direction (spaceline--direction ,dir) ,dir))
             (sep spaceline-separator-type))
         (propertize (all-the-icons-alltheicon (format "%s-%s" sep dir) :v-adjust 0.0)
                     'face `(:height 1.6
                             :family
                             ,(all-the-icons-alltheicon-family)
                             :foreground
                             ,(face-attribute ,start-face :background)
                             :background
                             ,(face-attribute ,end-face :background))))
       :skip-alternate t :tight t :when (if ,invert (not active) active))))

(defvar spaceline-invert-direction t)
(defvar spaceline-separator-type "slant")

(define-separator "right-1" "right" 'default 'highlight)
(define-separator "right-2" "right" 'highlight 'default)

;; Face function
(setq spaceline-face-func (lambda (face active)
                            (if active
                                (cond ((eq face 'line) (spacemacs//evil-state-face))
                                      ((eq face 'face1) (spacemacs//evil-state-face))
                                      ((eq face 'face2) (spacemacs//evil-state-face))
                                      ((eq face 'highlight) 'highlight)
                                      (t 'default))
                              'font-lock-comment-face)))

(spaceline-compile
  "ati"
  '(((ati-modified ati-buffer-size) :face highlight-face :skip-alternate t)
    ati-right-2-separator
    ((ati-persp-name ati-projectile ati-mode-icon ati-buffer-id))
    ati-right-1-separator
    ((ati-position ati-region-info) :separator "  " :face highlight-face)
    ati-right-2-separator
    ((ati-vc-icon) :face other-face)
    ati-right-1-separator
    ((ati-flycheck-status
      ((ati-org-clock :when active)
       (ati-org-pomodoro :when active))) :separator "  " :face highlight-face)
    ati-right-2-separator)

  '(ati-right-1-separator
    ((ati-buffer-position
      ati-time) :separator "  " :face highlight-face)))

(provide 'spaceline-all-the-icons)
;;; spaceline-all-the-icons.el ends here
