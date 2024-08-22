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

(provide 'r-writing)

;;; r-writing.el ends here
