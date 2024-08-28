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

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook (lambda () (setq line-spacing 0.2)))

(use-package markdown-mode)

(use-package org-modern
  :custom
  (org-hide-emphasis-markers t)
  (org-catch-invisible-edits 'show-and-error)
  (org-pretty-entities t)
  (org-modern-checkbox nil)
  ;; (org-modern-checkbox '((?X . "🟩")
  ;;                        (?- . "🔳")
  ;;                        (?\s . "⬜")))
  (org-modern-star nil)
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

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package org-margin
  :vc (:fetcher github :repo rougier/org-margin)
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
  :custom
  (org-roam-directory user-notes-dir)
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
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
                                                        "#+TITLE: ${title}\n\n")
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

;; Primarily for serving `pile' pages
(use-package w
  :vc (:fetcher github :repo lepisma/w.el))

;; This is for some template rendering for `pile'
(use-package mustache)

(use-package pile
  :vc (:fetcher github :repo lepisma/pile)
  :after (mustache w)
  :config
  (let* ((template-dir (concat  "misc/"))
         (preamble-template "<header>
  <div class='site-title'>
    <a href='/'>
      <img src='/assets/images/avatar32.png'>
    </a>
  </div>
  <div class='site-nav'>
    <a {{#blog-p}}class='active'{{/blog-p}} href='/'> blog</a>
    <a {{#journal-p}}class='active'{{/journal-p}} href='/journal'> journal</a>
    <a {{#log-p}}class='active'{{/log-p}} href='/log'> log</a>
    <a {{#wiki-p}}class='active'{{/wiki-p}} href='/wiki'> wiki</a>
    <a href='/about'> about</a>
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
                                   :root-url root-url
                                   :base-url ""
                                   :input-dir (concat input-dir "blog")
                                   :output-dir output-dir
                                   :postamble postamble-template
                                   :preamble (mustache-render preamble-template '(("blog-p" . t) ("page-meta" . "%d")))
                                   :setupfile (concat input-dir "assets/export.setup"))
                (pile-project-blog :name "journal"
                                   :root-url root-url
                                   :base-url "journal"
                                   :input-dir (concat input-dir "journal")
                                   :output-dir (concat output-dir "journal")
                                   :postamble postamble-template
                                   :preamble (mustache-render preamble-template '(("journal-p" . t) ("page-meta" . "%d")))
                                   :setupfile (concat input-dir "assets/export.setup"))
                (pile-project-blog :name "log"
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
    (dolist (fn (list #'pile-hooks-post-clear-cids
                      #'pile-hooks-post-generate-atom
                      #'pile-hooks-post-generate-archive
                      #'pile-hooks-post-stringify-title
                      #'pile-hooks-post-sync-static-files
                      #'pile-hooks-post-generate-index))
      (add-hook 'pile-post-publish-hook fn t))

    (defun pile-status ()
        "Show `magit-status' in the git tracked output directory."
        (interactive)
        (magit-status output-dir))))

(provide 'r-writing)

;;; r-writing.el ends here
