;;; rogue-org --- Org mode settings for rogue layer

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
;; Keywords: org, rogue
;; URL: https://github.com/lepisma/rogue/tree/master/local/rogue-org

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

(require 'f)
(require 'helm-bibtex)
(require 'org)
(require 'org-ref)

(defun rogue-org-setup-tex ()
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
                       :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
                       :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O")))
        org-format-latex-options
        '(:foreground "Black" :background "Transparent" :scale 1.0
                      :html-foreground "Black" :html-background "Transparent"
                      :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

  ;; Setup helm bibtex action for opening pdf
  (let ((pdf-action "Open local pdf"))
    (helm-delete-action-from-source pdf-action helm-source-bibtex)
    (helm-add-action-to-source pdf-action
                               (lambda (key)
                                 (let ((file-path (concat (f-join user-pdfs-dir key) ".pdf")))
                                   (if (f-exists? file-path)
                                       (find-file file-path)
                                     (message (format "Pdf not found for %s" key)))))
                               helm-source-bibtex)))

(defun rogue-org-setup-babel ()
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
     (sagemath   . t)
     (scheme     . t)
     (sh         . t)
     (shell      . t)
     (sql        . t)
     (sqlite     . t))))

;;;###autoload
(defun rogue-org-shuffle-save ()
  "Shuffle and save current file"
  (interactive)
  (goto-char (point-min))
  (org-sort-entries nil ?f (lambda () (random 1000)))
  (save-buffer))

(defun rogue-org-reset-buffers ()
  "Reset org-mode in all org buffers"
  (dolist (buff (buffer-list))
    (with-current-buffer buff
      (if (string-equal "org-mode" major-mode)
          (org-mode)))))

;;;###autoload
(defun rogue-org-clock-in ()
  "Default clock in clock.org"
  (interactive)
  (with-current-buffer "clock.org"
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (org-clock-in)
    (org-save-all-org-buffers)))

;;;###autoload
(defun rogue-org-clock-out ()
  "Default clock in clock.org"
  (interactive)
  (with-current-buffer "clock.org"
    (org-clock-out)
    (org-save-all-org-buffers)))

(defun rogue-org-setup-notes ()
  "Setup agenda/captures and other notes related things"

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (with-eval-after-load 'org
    ;; Capture templates
    (setq org-directory user-notes-dir
          org-capture-templates
          '(("g" "Google calender event" entry (file user-gcal-file)
             "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
            ("l" "Logistic" entry (file "notes.org")
             "* LOGISTICS %?\nSCHEDULED: %^T\n")
            ("n" "Note" entry (file "notes.org")
             "* %?\n")
            ("b" "Bookmark" entry (file "notes.org")
             "* %?\n%a")))

    (setq org-html-validation-link nil)

    (setq org-refile-use-outline-path 'file
          org-outline-path-complete-in-steps nil
          org-refile-targets '((org-agenda-files :maxlevel . 1)))

    (setq org-agenda-custom-commands
          '(("n" "Main agenda view"
             ((agenda "")))
            ("l" "Logistics"
             ((todo "LOGISTICS"
                    ((org-agenda-overriding-header "Logistics")
                     (org-agenda-sorting-strategy '(priority-down))))))
            ("d" "Upcoming deadlines" agenda ""
             ((org-agenda-entry-types '(:deadline))
              (org-deadline-warning-days 30)
              (org-agenda-time-grid nil)
              (org-agenda-overriding-header "Upcoming deadlines")))))))

(defun rogue-org-setup-general ()
  "Misc settings."

  (with-eval-after-load 'org
    (setq org-startup-indented t
          org-clock-idle-time 5
          org-bullets-bullet-list '("➜")
          org-ellipsis "  "
          org-pretty-entities t
          org-hide-emphasis-markers t
          org-agenda-block-separator ""
          org-fontify-whole-heading-line t
          org-fontify-done-headline t
          org-fontify-quote-and-verse-blocks t
          spaceline-org-clock-p t)

    (customize-set-variable 'org-modules
                            '(org-bibtex
                              org-docview
                              org-habit
                              org-info
                              org-w3m))))

(provide 'rogue-org)

;;; rogue-org.el ends here
