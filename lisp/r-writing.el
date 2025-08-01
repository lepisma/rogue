;;; r-writing.el --- Writing setup -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;; Writing setup
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
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'org-mode-hook (lambda () (setq line-spacing 0.2)))

(use-package org
  :custom
  (org-log-done 'time)
  :config
  (require 'org-tempo)

  ;; Both the regex based functions don't start matching from the start of the
  ;; line. This could cause issues at some point in time.

  (defun org-checkbox-state ()
    "Return checkbox state for checkbox list item in current line."
    (save-excursion
      (goto-char (line-beginning-position))
      (if (re-search-forward " \\[X\\] " (line-end-position) t)
          'checked
        (if (re-search-forward " \\[-\\] " (line-end-position) t)
            'partial
          (if (re-search-forward " \\[ \\] " (line-end-position) t)
              'unchecked
            'unknown)))))

  (defun org-checkbox-done-timestamp-range ()
    "Return range of done timestamp if marked in the checklist item."
    (let ((timestamp-regex " \\[[ X-]\\] \\(\\[[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\} \\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\) [[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}\\] \\)"))
      (save-excursion
        (goto-char (line-beginning-position))
        (when (re-search-forward timestamp-regex (line-end-position) t)
          (cons (match-beginning 1) (match-end 1))))))

  (defun org-checkbox-date-fn ()
    "Add a timestamp when the checklist is moved to done state.

Ignore if the content already has a timestamp (of a certain fixed
format). In case the checklist is marked as not done or partially done,
remove the present timestamp, if any."
    (save-excursion
      (let ((checkbox-state (org-checkbox-state))
            (timestamp-range (org-checkbox-done-timestamp-range)))
        (if (eq checkbox-state 'checked)
            (unless timestamp-range
              ;; We go just after the marked checklist
              (goto-char (line-beginning-position))
              (re-search-forward " \\[X\\] " (line-end-position) t)
              (org-insert-timestamp (current-time) t t)
              (insert " "))
          (when timestamp-range
            (delete-region (car timestamp-range) (cdr timestamp-range)))))))

  (add-hook 'org-checkbox-statistics-hook #'org-checkbox-date-fn))

(use-package markdown-mode)

(use-package org-modern
  :custom
  (org-hide-emphasis-markers t)
  (org-catch-invisible-edits 'show-and-error)
  (org-pretty-entities t)
  (org-modern-checkbox nil)
  (org-modern-todo nil)
  (org-modern-priority nil)
  (org-modern-tag nil)
  (org-modern-star nil)
  (org-modern-timestamp nil)
  (org-modern-horizontal-rule nil)
  (org-modern-table-vertical 1)

  :config
  (global-org-modern-mode))

(use-package org-variable-pitch
  :hook (org-mode . org-variable-pitch-minor-mode))

(use-package org-appear
  :custom
  (org-appear-autolinks t)
  (org-appear-autoemphasis t)
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t)
  (org-appear-autokeywords t)

  :hook (org-mode . org-appear-mode))

(use-package org-present
  :hook ((org-present-mode . (lambda ()
                               (org-present-big)
                               (org-display-inline-images)
                               (org-present-hide-cursor)
                               (org-present-read-only)))
         (org-present-mode-quit . (lambda ()
                                    (org-present-small)
                                    (org-remove-inline-images)
                                    (org-present-show-cursor)
                                    (org-present-read-write)))))

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package svg-tag-mode
  :config
  (let* ((date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
         (time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
         (day-re "[A-Za-z]\\{3\\}")
         (day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re)))

    (defun svg-progress-percent (value)
      (save-match-data
        (svg-image (svg-lib-concat
                    (svg-lib-progress-bar  (/ (string-to-number value) 100.0)
                                           nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                    (svg-lib-tag (concat value "%")
                                 nil :strok e 0 :margin 0)) :ascent 'center)))

    (defun svg-progress-count (value)
      (save-match-data
        (let* ((seq (split-string value "/"))
               (count (if (stringp (car seq))
                          (float (string-to-number (car seq)))
                        0))
               (total (if (stringp (cadr seq))
                          (float (string-to-number (cadr seq)))
                        1000)))
          (svg-image (svg-lib-concat
                      (svg-lib-progress-bar (/ count total) nil
                                            :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                      (svg-lib-tag value nil
                                   :stroke 0 :margin 0)) :ascent 'center))))

    (setq svg-tag-tags
          `(;; Org tags
            (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
            (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

            ;; Task priority
            ("\\[#[A-Z]\\]" . ((lambda (tag)
                                 (svg-tag-make tag :face 'org-priority
                                               :beg 2 :end -1 :margin 0))))

            ;; TODO / DONE
            ("TODO" . ((lambda (tag) (svg-tag-make tag :face 'org-todo :inverse t :margin 0))))
            ("NEXT" . ((lambda (tag) (svg-tag-make tag :face 'org-todo :inverse t :margin 0))))
            ("ACTIVE" . ((lambda (tag) (svg-tag-make tag :face 'org-todo :inverse t :margin 0))))
            ("DONE" . ((lambda (tag) (svg-tag-make tag :face 'org-done :inverse t :margin 0))))


            ;; Citation of the form [cite:@Knuth:1984]
            ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                              (svg-tag-make tag
                                                            :inverse t
                                                            :beg 7 :end -1
                                                            :crop-right t))))
            ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                       (svg-tag-make tag
                                                                     :end -1
                                                                     :crop-left t))))

            ;; Active date (with or without day name, with or without time)
            (,(format "\\(<%s>\\)" date-re) .
             ((lambda (tag)
                (svg-tag-make tag :beg 1 :end -1 :margin 0))))
            (,(format "\\(<%s \\)%s>" date-re day-time-re) .
             ((lambda (tag)
                (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
            (,(format "<%s \\(%s>\\)" date-re day-time-re) .
             ((lambda (tag)
                (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

            ;; Inactive date  (with or without day name, with or without time)
            (,(format "\\(\\[%s\\]\\)" date-re) .
             ((lambda (tag)
                (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
            (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
             ((lambda (tag)
                (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
            (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
             ((lambda (tag)
                (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))

            ;; ;; Progress
            ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                                (svg-progress-percent (substring tag 1 -2)))))
            ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                              (svg-progress-count (substring tag 1 -1))))))))

  :hook (org-mode . svg-tag-mode))

(use-package org-margin
  :vc (:url "https://github.com/rougier/org-margin.git" :rev :newest)
  :custom
  (org-margin-headers-set 'H-svg)
  (org-margin-headers
   (list (cons 'stars (list (propertize "     #" 'face '(fixed-pitch default))
                            (propertize "    ##" 'face '(fixed-pitch default))
                            (propertize "   ###" 'face '(fixed-pitch default))
                            (propertize "  ####" 'face '(fixed-pitch default))
                            (propertize " #####" 'face '(fixed-pitch default))
                            (propertize "######" 'face '(fixed-pitch default))))
         (cons 'H-txt (list (propertize "H1" 'face '(font-lock-comment-face default))
                            (propertize "H2" 'face '(font-lock-comment-face default))
                            (propertize "H3" 'face '(font-lock-comment-face default))
                            (propertize "H4" 'face '(font-lock-comment-face default))
                            (propertize "H5" 'face '(font-lock-comment-face default))
                            (propertize "H6" 'face '(font-lock-comment-face default))))
         (cons 'H-svg (list (svg-lib-tag "H1" '(org-level-1))
                            (svg-lib-tag "H2" '(org-level-2))
                            (svg-lib-tag "H3" '(org-level-3))
                            (svg-lib-tag "H4" '(org-level-4))
                            (svg-lib-tag "H5" '(org-level-5))
                            (svg-lib-tag "H6" '(org-level-6))))))
  (org-margin-markers
   (list (cons "\\(#\\+begin_src\\)"
               (propertize " " 'face '(font-lock-comment-face bold)))
         (cons "\\(#\\+begin_quote\\)"
               (propertize " " 'face '(font-lock-comment-face bold)))))

  :hook (org-mode . org-margin-mode))

(use-package org-roam
  :demand t
  :custom
  (org-roam-directory user-notes-dir)
  (org-roam-node-display-template "${title}")
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-capture-templates `(("n" "default" plain "%?"
                                 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                    "#+TITLE: ${title}\n\n")
                                 :unnarrowed)
                                ("l" "literature" plain "%?"
                                 :target (file+head "literature/%<%Y%m%d%H%M%S>-${slug}.org"
                                                    "#+TITLE: ${title}\n\n")
                                 :unnarrowed)))
  (org-roam-capture-ref-templates `(("l" "literature" plain "%?"
                                     :target (file+head "literature/%<%Y%m%d%H%M%S>-${slug}.org"
                                                        "#+TAGS: unsorted\n#+TITLE: ${title}\n\n")
                                     :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (require 'org-roam-protocol)
  (org-roam-db-autosync-mode))

(use-package tokenizers
  :vc (:url "https://github.com/lepisma/tokenizers.el.git" :rev :newest)
  :demand t)

(use-package onnx
  :vc (:url "https://github.com/lepisma/onnx.el.git" :rev :newest)
  :demand t)

(use-package sem
  :vc (:url "https://github.com/lepisma/sem.el.git" :rev :newest)
  :demand t
  :config
  ;; Create database directory
  (setq sem-data-dir (expand-file-name (concat user-emacs-directory "sem/")))
  (make-directory sem-data-dir t)

  ;; Setup default model
  (setq sem-embed-model-path (concat sem-data-dir "model_O2.onnx"))
  (unless (file-exists-p sem-embed-model-path)
    (message "Model missing for sem-embed, downloading...")
    (url-copy-file "https://huggingface.co/sentence-transformers/all-MiniLM-L6-v2/resolve/main/onnx/model_O2.onnx?download=true" sem-embed-model-path)))

(use-package org-roam-exts
  :after org-roam
  :vc (:url "https://github.com/lepisma/org-roam-exts.git" :rev :newest)
  :config
  ;; Ensure that the side buffer is positioned in the right way
  (add-to-list 'display-buffer-alist
           '("\\*org-roam\\*"
             (display-buffer-in-direction)
             (direction . right)
             (window-width . 0.33)
             (window-height . fit-window-to-buffer)))

  ;; Enable rich link preview and similar nodes in org-roam-buffer
  (org-roam-buffer-exts-enable)
  ;; Enable org-protocol for sidekick functionality
  (org-roam-sk-enable)
  :bind (("C-c n s" . org-roam-sem-search)))

;; Primarily for serving `pile' pages
(use-package w
  :vc (:url "https://github.com/lepisma/w.el.git" :rev :newest)
  :demand t
  :ensure-system-package (live-server . "cargo install live-server"))

(use-package org-books
  :custom
  (org-books-file (concat user-cloud-dir "lepisma.github.io/wiki/readings/reading-list.org")))

;; This is for some template rendering for `pile'
(use-package mustache
  :demand t)

(use-package pile
  ;; :vc (:url "https://github.com/lepisma/pile.git" :rev :newest)
  :load-path (lambda () (concat user-cloud-dir "projects/pile/"))
  :after (mustache w transient magit)
  :demand t
  :commands (pile-publish-current-file pile-serve pile-status pile-blog-new-post)
  :config
  (let ((preamble-template "<header>
  <div class='site-title'>
    <a href='/'>
      <img src='/assets/images/avatar32.png'>
    </a>
  </div>
  <div class='site-nav'>
    <a {{#blog-p}}class='active'{{/blog-p}} href='/'> blog</a>
    <a {{#journal-p}}class='active'{{/journal-p}} href='/journal'> journal</a>
    <a {{#log-p}}class='active'{{/log-p}} href='/log'> <s>log</s></a>
    <a {{#wiki-p}}class='active'{{/wiki-p}} href='/wiki'> wiki</a>
    <a href='/about'> about</a>
    <a href='/wiki/support.html' class='btn small'>$ support oss</a>
  </div>
  <div class='clearfix'></div>
</header>

<div class='page-header'>
  <div class='page-meta'>{{page-meta}}</div>
  <h1>%t</h1>
</div>")
        (postamble-template "<footer id='footer'></footer>")
        (root-url "https://lepisma.xyz/")
        (input-dir (concat user-cloud-dir "lepisma.github.io/"))
        (output-dir (concat user-cloud-dir "projects/lepisma.github.io-deploy/")))
    (setq pile-serve-dir output-dir
          pile-projects
          (list (pile-project-wiki :name "wiki"
                                   :root-url root-url
                                   :base-url "wiki"
                                   :input-dir (concat input-dir "wiki")
                                   :output-dir (concat output-dir "wiki")
                                   :postamble postamble-template
                                   :preamble (mustache-render preamble-template '(("wiki-p" . t) ("page-meta" . "Last modified: %d %C")))
                                   :setupfile (concat input-dir "assets/export.setup"))
                (pile-project-blog :name "blog"
                                   :site-title "(car nil)"
                                   :root-url root-url
                                   :base-url ""
                                   :input-dir (concat input-dir "blog")
                                   :output-dir output-dir
                                   :postamble postamble-template
                                   :preamble (mustache-render preamble-template '(("blog-p" . t) ("page-meta" . "%d")))
                                   :setupfile (concat input-dir "assets/export.setup"))
                (pile-project-blog :name "journal"
                                   :site-title "journal | lepisma"
                                   :root-url root-url
                                   :base-url "journal"
                                   :input-dir (concat input-dir "journal")
                                   :output-dir (concat output-dir "journal")
                                   :postamble postamble-template
                                   :preamble (mustache-render preamble-template '(("journal-p" . t) ("page-meta" . "%d")))
                                   :setupfile (concat input-dir "assets/export.setup"))
                (pile-project-blog :name "log"
                                   :site-title "log | lepisma"
                                   :root-url root-url
                                   :base-url "log"
                                   :input-dir (concat input-dir "log")
                                   :output-dir (concat output-dir "log")
                                   :postamble postamble-template
                                   :preamble (mustache-render preamble-template '(("log-p" . t) ("page-meta" . "%d"))))
                (pile-project-static :name "assets"
                                     :root-url root-url
                                     :input-dir (concat input-dir "assets")
                                     :output-dir (concat output-dir "assets"))
                (pile-project-plain :name "misc"
                                    :root-url root-url
                                    :base-url ""
                                    :input-dir (concat input-dir "misc")
                                    :output-dir output-dir)))
    (pile-setup)
    (setq org-confirm-babel-evaluate nil)
    ;; Notice that sequence of hooks is important here
    (dolist (fn (list #'pile-hooks-pre-add-setupfile
                      #'pile-hooks-pre-add-bc
                      #'pile-hooks-pre-add-cids
                      #'pile-hooks-pre-add-date
                      #'pile-hooks-pre-add-dropcap
                      #'pile-hooks-pre-add-tags
                      #'pile-hooks-pre-add-crosslinks
                      #'pile-hooks-pre-add-draft-watermark))
      (add-hook 'pile-pre-publish-hook fn t))
    (dolist (fn (list (pile-hooks-html-post-processing-factory (list #'pile-cids-clear-html
                                                                     #'pile-stringify-title))
                      #'pile-hooks-post-generate-atom
                      #'pile-hooks-post-generate-archive
                      #'pile-hooks-post-sync-static-files
                      #'pile-hooks-post-generate-index))
      (add-hook 'pile-post-publish-hook fn t))

    ;; Set browser shortcut for editing
    (require 'org-protocol)
    (push (list "org-open-source" :protocol "open-source" :function #'org-protocol-open-source)
          org-protocol-protocol-alist)
    (push `("pile-blog"
            :base-url ,root-url
            :working-directory ,(concat input-dir "blog/")
            :online-suffix ".html"
            :working-suffix ".org")
          org-protocol-project-alist)

    (defun pile-status ()
      "Show `magit-status' in the git tracked output directory."
      (interactive)
      (magit-status output-dir)))

  (defun pile-commit-and-push ()
    "Commit staged changes with a generic message and push to remote.

This is supposed to be used only with pile but there is no check
for that right now."
    (interactive)
    (magit-run-git "commit" "-m" "Updates")
    (magit-run-git-async "push"))

  (transient-append-suffix 'magit-commit "c"
    '("g" "Pile commit and push" pile-commit-and-push)))

(use-package org-cliplink
  :demand t
  :preface
  (defun org-cliplink-plus ()
    "When a region is selected, just wrap it in the link instead of
trying to fetch title."
    (interactive)
    (if (region-active-p)
        (replace-region-contents (region-beginning) (region-end)
                                 (lambda ()
                                   (format "[[%s][%s]]" (org-cliplink-clipboard-content) (buffer-substring-no-properties (point-min) (point-max)))))
      (org-cliplink)))

  :bind (:map org-mode-map
              ("C-c y" . org-cliplink-plus)))

(use-package citar
  :no-require
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-templates '((main . "${author editor:30%sn}     ${date year issued:4}     ${title:48}")
                     (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
                     (preview . "${author editor} (${year issued date}) [[${url}][\"${title}\"]], in /${journal journaltitle booktitle eventtitle publisher container-title collection-title}/\n")
                     (note . "Notes on ${author editor:%etal}, ${title}")))
  :bind (:map org-mode-map
              ("C-c b" . org-cite-insert)))

(use-package esi-dictate
  :vc (:url "https://github.com/lepisma/emacs-speech-input.git" :rev :newest)
  :custom
  (esi-dictate-dg-api-key (auth-info-password (car (auth-source-search :host "deepgram"))))
  (esi-dictate-llm-provider (make-llm-openai :key (auth-info-password (car (auth-source-search :host "api.openai.com"))) :chat-model "gpt-4o-mini"))
  :config
  (setq llm-warn-on-nonfree nil)
  :hook (esi-dictate-speech-final . esi-dictate-fix-context))

(use-package org-download
  :custom
  (org-download-method 'directory)
  (org-download-image-dir ".")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y-%m-%dT%H:%M:%S-"))

(provide 'r-writing)

;;; r-writing.el ends here
