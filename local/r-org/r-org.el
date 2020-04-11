;;; r-org --- Org mode settings for rogue layer

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Personal config package for setting up org mode
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

(require 'dash)
(require 'f)
(require 'helm-bibtex)
(require 'hydra)
(require 'org)
(require 'org-ref)
(require 'org-tempo)
(require 'org-pomodoro)
(require 'pile)

;; A few extra actions for helm-bibtex

(defun helm-bibtex-open-local-pdf (key)
  (let ((file-path (concat (f-join user-pdfs-dir key) ".pdf")))
    (if (f-exists? file-path)
        (find-file file-path)
      (message (format "Pdf not found for %s" key)))))

(defun helm-bibtex-candidate-get (key item)
  (alist-get key (cdr item) nil nil #'string-equal))

(defun helm-bibtex-insert-notes-template (key)
  (let ((item (-find (lambda (it) (string-equal key (helm-bibtex-candidate-get "=key=" it)))
                     (bibtex-completion-candidates))))
    (org-insert-heading)
    (insert (helm-bibtex-candidate-get "title" item))
    (insert "\n")
    (org-set-property "CUSTOM_ID" (helm-bibtex-candidate-get "=key=" item))
    (let ((pairs-to-insert (-remove (lambda (pair)
                                      (or (s-starts-with? "=" (car pair))
                                          (member (car pair) '("title"))))
                                    (cdr item))))
      (dolist (pair pairs-to-insert)
        (org-set-property (upcase (car pair)) (cdr pair))))))

(defun helm-bibtex-insert-review-line (key)
  (let ((item (-find (lambda (it) (string-equal key (helm-bibtex-candidate-get "=key=" it)))
                     (bibtex-completion-candidates))))
    (insert (helm-bibtex-candidate-get "title" item) " (cite:" key ")")))

(defun r-org/setup-tex ()
  "Setup tex related stuff."
  (setq bib-library user-bib-file
        reftex-default-bibliography (list user-bib-file)
        org-ref-default-bibliography (list user-bib-file)
        bibtex-completion-bibliography user-bib-file
        org-ref-bibliography-notes user-bib-notes-file
        bibtex-completion-notes-path user-bib-notes-file
        org-ref-pdf-directory user-pdfs-dir
        org-ref-notes-function 'org-ref-notes-function-one-file)

  (setq org-latex-pdf-process (list "latexmk -pdflatex=xelatex -f -pdf %f"))
  (setq TeX-engine 'xetex)

  (setq org-preview-latex-default-process 'imagemagick
        org-preview-latex-process-alist
        '((imagemagick :programs ("xelatex" "convert")
                       :description "pdf > png"
                       :message "you need to install the programs: xelatex and imagemagick."
                       :use-xcolor t
                       :image-input-type "pdf"
                       :image-output-type "png"
                       :image-size-adjust (1.0 . 1.0)
                       :latex-compiler ("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")
                       :image-converter ("convert -density 300 -trim -antialias %f -quality 100 %O")))
        org-format-latex-options
        '(:foreground "Black" :background "Transparent" :scale 1.0
                      :html-foreground "Black" :html-background "Transparent"
                      :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

  ;; Remove unused bibtex actions
  (let ((actions (list "Open PDF, URL or DOI"
                       "Edit notes"
                       "Add PDF to library")))
    (dolist (action actions)
      (helm-delete-action-from-source action helm-source-bibtex)))

  ;; Setup helm bibtex action for opening pdf
  (let ((actions '(("Open local pdf" . helm-bibtex-open-local-pdf)
                   ("Insert notes template" . helm-bibtex-insert-notes-template)
                   ("Insert review line" . helm-bibtex-insert-review-line))))
    (dolist (action actions)
      (helm-delete-action-from-source (car action) helm-source-bibtex)
      (helm-add-action-to-source (car action) (cdr action) helm-source-bibtex))))

(defun r-org/setup-babel ()
  "Setup org-babel."
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C          . t)
     (calc       . t)
     (comint     . t)
     (ditaa      . t)
     (dot        . t)
     (emacs-lisp . t)
     (gnuplot    . t)
     (haskell    . t)
     (js         . t)
     (latex      . t)
     (lisp       . t)
     (makefile   . t)
     (python     . t)
     (R          . t)
     (restclient . t)
     (ruby       . t)
     (scheme     . t)
     (shell      . t)
     (sql        . t)
     (sqlite     . t))))

(defun r-org/reset-buffers ()
  "Reset org-mode in all org buffers"
  (dolist (buff (buffer-list))
    (with-current-buffer buff
      (if (string-equal "org-mode" major-mode)
          (org-mode)))))

(defun r-org/setup-notes ()
  "Setup agenda/captures and other notes related things"

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (with-eval-after-load 'org
    ;; Capture templates
    (setq org-directory user-notes-dir
          org-capture-templates
          `(("n" "Note" entry (file ,(concat user-notes-dir "personal/notes.org"))
             "* %?\nSCHEDULED: %^t" :empty-lines 1)
            ("b" "Bookmark" entry (file ,(concat user-notes-dir "personal/notes.org"))
             "* %?\n%a" :empty-lines 1)

            ;; Minor logs
            ("l" "Log")
            ("lw" "Weekly log" item (file+olp ,(concat user-notes-dir "personal/notes.org") "Weekly review" "Done")
             "- %U %?" :empty-lines-after 1)
            ("ll" "Logistics" entry (file+olp ,(concat user-notes-dir "personal/notes.org") "Logistics")
             "* %?\nSCHEDULED: %^t" :empty-lines 1 :prepend t)))

    (setq org-html-validation-link nil)

    (setq org-refile-use-outline-path 'file
          org-outline-path-complete-in-steps nil
          org-refile-targets `((org-agenda-files :maxlevel . 1)
                               (,(concat user-notes-dir "work/main.org") :maxlevel . 1)))

    (setq org-agenda-custom-commands
          `(("n" "Personal agenda"
             ((agenda "")
              (alltodo))
             ((org-super-agenda-groups
               '((:name "Travel"
                        :tag "travel")
                 (:name "Logistics"
                        :tag "logistics")
                 (:name "Jams"
                        :tag "jam")
                 (:name "TODO"
                        :todo ("TODO" "NOW"))
                 (:name "Readings"
                        :todo "READING")
                 (:auto-category t)))
              (org-agenda-files (list ,@(directory-files-recursively (concat user-notes-dir "personal") org-agenda-file-regexp)
                                      ,(concat user-notes-dir "personal/medical.org.gpg")
                                      ,(concat user-notes-dir "incoming/captures.org")
                                      ,(pile-path-abs "wiki:readings/reading-list")
                                      ,(pile-path-abs "wiki:readings/notes/documents")))))
            ("c" "Humans"
             ((agenda "")
              (alltodo))
             ((org-agenda-files (list ,(concat user-notes-dir "personal/humans.org.gpg")))))
            ("w" "Work agenda"
             ((agenda "")
              (alltodo))
             ((org-super-agenda-groups
               '((:name "Important"
                        :priority "A")))
              (org-agenda-files (list ,@(directory-files-recursively (concat user-notes-dir "work") org-agenda-file-regexp)
                                      ,(concat user-notes-dir "incoming/captures.org")))))))))

(defun r-org/cliplink-to-region ()
  "Add link from clipboard to the region."
  (interactive)
  (if (region-active-p)
      (let ((url (substring-no-properties (current-kill 0)))
            (text (buffer-substring-no-properties (region-beginning) (region-end))))
        (save-excursion
          (delete-active-region)
          (insert (format "[[%s][%s]]" url text))))
    (message "No region active")))

(defun r-org/clock-in ()
  "Clock in starting with the list view."
  (interactive)
  (if (null (-filter #'marker-buffer org-clock-history))
      (org-pomodoro)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'org-pomodoro))))

(defun r-org/clock-out ()
  (interactive)
  (org-pomodoro-kill))

;;;###autoload
(defun r-org/setup-general ()
  "Misc settings."
  (setq org-startup-indented t
        org-clock-idle-time 5
        org-bullets-bullet-list '("› ")
        org-ellipsis "  "
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-agenda-block-separator ""
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-catch-invisible-edits 'show-and-error
        org-cycle-separator-lines 0
        spaceline-org-clock-p t
        org-modules '(org-bibtex
                      org-docview
                      org-habit
                      org-info))

  (r-utils/add-hooks
   '(org-pomodoro-started-hook
     org-pomodoro-finished-hook
     org-pomodoro-killed-hook
     org-archive-hook)
   (list #'org-save-all-org-buffers))
  (setq org-pomodoro-keep-killed-pomodoro-time t)

  (defhydra hydra-clock (global-map "C-c w" :exit t)
    ("i" r-org/clock-in "clock in")
    ("c" org-pomodoro "clock in current")
    ("o" r-org/clock-out "clock out")))

(provide 'r-org)

;;; r-org.el ends here
