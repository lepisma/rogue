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

(require 'org)

(defun rogue-org-setup-tex ()
  "Setup tex related stuff."
  (setq bib-library "~/library.bib")
  (setq reftex-default-bibliography '(bib-library)
        org-ref-default-bibliography '(bib-library)
        bibtex-completion-bibliography bib-library)
  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

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
     (ocaml      . t)
     (python     . t)
     (R          . t)
     (restclient . t)
     (ruby       . t)
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
  "Setup agenda/captures and other notes related things. Expects following variables
to be set:
  - user-journal-dir
  - user-gcal-file
  - user-bookmarks-file
  - user-books-file"

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (with-eval-after-load 'org
    ;; Capture templates
    (setq org-directory user-journal-dir
          org-capture-templates
          '(("g" "Google calender event" entry (file user-gcal-file)
             "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
            ("y" "Yak" entry (file "yak.org")
             "* %?")
            ("e" "Explore" entry (file "explore.org")
             "* %? %^g")
            ("t" "Todo" entry (file "notes.org")
             "* TODO %?\nSCHEDULED: %^T\n")
            ("b" "Bookmark" entry (file "notes.org")
             "* TODO %?\n%a")
            ("d" "Deadline" entry (file "notes.org")
             "* %?\nDEADLINE: %^T")))

    (setq org-refile-use-outline-path 'file
          org-outline-path-complete-in-steps nil
          org-refile-targets '((org-agenda-files :maxlevel . 1)))

    (setq org-agenda-custom-commands
          '(("n" "Main agenda view"
             ((tags "next"
                    ((org-agenda-overriding-header "Next concrete todos")))
              (agenda "")
              (todo "TODO")))
            ("r" "Readings"
             ((todo "READING"
                    ((org-agenda-files (list user-bookmarks-file))
                     (org-agenda-overriding-header "Bookmarks")))
              (todo "READING"
                    ((org-agenda-files (list user-books-file))
                     (org-agenda-overriding-header "Books")))
              (todo "NEXT"
                    ((org-agenda-files (list user-books-file))
                     (org-agenda-overriding-header "Upcoming books")))))
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
          org-bullets-bullet-list '("#")
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
