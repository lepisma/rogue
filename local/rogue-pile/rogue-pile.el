;;; rogue-pile.el --- Pile management

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/lepisma/rogue-pile.el

;;; Commentary:

;; Org pile management
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
(require 'ox-html)
(require 'ox-publish)

(defun rogue-pile-setup ()
  "Setup for pile"
  (let ((pile-source (concat user-project-dir "pile/pile/"))
        (pile-output (concat user-project-dir "pile/docs/"))
        (preamble "<header>
  <div class='site-title'>
    <a href='/'>
      <img src='/assets/images/avatar32.png'>
    </a>
  </div>
  <div class='site-nav'>
    <a href='/pile'> pile</a>
    <a href='/feed.xml'> feed</a>
    <a href='/archive'> blog</a>
    <a href='/about'> about</a>
  </div>
  <div class='clearfix'>
  </div>
</header>

<div class='page-header'>
  <div class='page-meta small'>
    Last modified: %d %C
  </div>
  <h1>%t</h1>
</div>")
        (postamble "<footer></footer>"))
    (setq org-publish-project-alist
          `(("pile-pages"
             :auto-sitemap t
             :sitemap-filename "sitemap.org"
             :sitemap-title "Sitemap"
             :base-directory ,pile-source
             :base-extension "org"
             :recursive t
             :publishing-directory ,pile-output
             :publishing-function org-html-publish-to-html
             :htmlized-source nil
             :html-checkbox-type unicode
             :html-doctype "html5"
             :html-html5-fancy t
             :html-postamble ,postamble
             :html-preamble ,preamble)
            ("pile-static"
             :base-directory ,pile-source
             :base-extension ".*"
             :exclude ".*\.org"
             :recursive t
             :publishing-directory ,pile-output
             :publishing-function org-publish-attachment)
            ("pile" :components ("pile-pages" "pile-static"))))))

(provide 'rogue-pile)

;;; rogue-pile.el ends here
