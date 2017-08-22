;;; config.el --- rogue Layer config File for Spacemacs

(defvar rogue-current-color 'dark
  "Currently active color scheme")

(let* ((bg-white           "#fbf8ef")
       (bg-light           "#222425")
       (bg-dark            "#1c1e1f")
       (bg-darker          "#1c1c1c")
       (fg-white           "#ffffff")
       (shade-white        "#efeae9")
       (fg-light           "#655370")
       (dark-cyan          "#008b8b")
       (region-dark        "#2d2e2e")
       (region             "#39393d")
       (slate              "#8FA1B3")
       (keyword            "#f92672")
       (comment            "#525254")
       (builtin            "#fd971f")
       (purple             "#9c91e4")
       (doc                "#727280")
       (type               "#66d9ef")
       (string             "#b6e63e")
       (gray               "#bbb")
       (sans-font          "Source Sans Pro")
       (serif-font         "Merriweather")
       (et-font            "EtBembo")
       (sans-mono-font     "Souce Code Pro")
       (serif-mono-font    "Verily Serif Mono"))
  (setq theming-modifications
        `((doom-molokai
           (variable-pitch
            :family ,sans-font)
           (eval-sexp-fu-flash
            :background ,dark-cyan
            :foreground ,fg-white)
           (eval-sexp-fu-flash-error
            :background ,keyword
            :foreground ,fg-white)
           (hackernews-link-face
            :foreground ,slate
            :inherit variable-pitch
            :height 1.2)
           (hackernews-comment-count-face
            :foreground ,string)
           (company-tooltip
            :background ,bg-darker)
           (company-scrollbar-fg
            :background ,comment)
           (company-scrollbar-bg
            :background ,bg-darker)
           (company-tooltip-common
            :foreground ,keyword)
           (company-tootip-annotation
            :foreground ,type)
           (company-tooltip-selection
            :background ,region)
           (show-paren-match
            :background ,keyword
            :foreground ,bg-dark)
           (magit-section-heading
            :foreground ,keyword)
           (magit-header-line
            :background nil
            :foreground ,bg-dark
            :box nil)
           (magit-diff-hunk-heading
            :background ,comment
            :foreground ,gray)
           (magit-diff-hunk-heading-highlight
            :background ,comment
            :foreground ,fg-white)
           (tooltip
            :foreground ,keyword
            :background ,bg-darker)
           (git-gutter-fr:modified
            :foreground ,dark-cyan)
           (doom-neotree-dir-face
            :foreground ,keyword
            :height 1.0)
           (doom-neotree-file-face
            :height 1.0)
           (doom-neotree-text-file-face
            :height 1.0)
           (doom-neotree-hidden-file-face
            :height 1.0
            :foreground ,comment)
           (doom-neotree-media-file-face
            :height 1.0
            :foreground ,type)
           (doom-neotree-data-file-face
            :height 1.0
            :foreground ,doc)
           (neo-root-dir-face
            :foreground ,fg-white
            :background ,region-dark
            :box (:line-width 6 :color ,region-dark))
           (mode-line
            :background ,bg-darker)
           (highlight
            :background ,region
            :foreground ,fg-white)
           (hl-line
            :background ,region-dark)
           (solaire-hl-line-face
            :background ,bg-dark)
           (org-document-title
            :inherit variable-pitch
            :height 1.3
            :weight normal
            :foreground ,gray)
           (org-document-info
            :foreground ,gray
            :slant italic)
           (org-level-1
            :inherit variable-pitch
            :height 1.3
            :weight bold
            :foreground ,keyword
            :background ,bg-dark)
           (org-level-2
            :inherit variable-pitch
            :weight bold
            :height 1.2
            :foreground ,gray
            :background ,bg-dark)
           (org-level-3
            :inherit variable-pitch
            :weight bold
            :height 1.1
            :foreground ,slate
            :background ,bg-dark)
           (org-level-4
            :inherit variable-pitch
            :weight bold
            :height 1.1
            :foreground ,slate
            :background ,bg-dark)
           (org-level-5
            :inherit variable-pitch
            :weight bold
            :height 1.1
            :foreground ,slate
            :background ,bg-dark)
           (org-level-6
            :inherit variable-pitch
            :weight bold
            :height 1.1
            :foreground ,slate
            :background ,bg-dark)
           (org-level-7
            :inherit variable-pitch
            :weight bold
            :height 1.1
            :foreground ,slate
            :background ,bg-dark)
           (org-level-8
            :inherit variable-pitch
            :weight bold
            :height 1.1
            :foreground ,slate
            :background ,bg-dark)
           (org-headline-done
            :strike-through t)
           (org-quote
            :background ,bg-dark)
           (org-block
            :background ,bg-dark)
           (org-block-begin-line
            :background ,bg-dark)
           (org-block-end-line
            :background ,bg-dark)
           (org-document-info-keyword
            :foreground ,comment)
           (org-link
            :underline nil
            :weight normal
            :foreground ,slate)
           (org-special-keyword
            :height 0.9
            :foreground ,comment)
           (org-todo
            :foreground ,builtin
            :background ,bg-dark)
           (org-done
            :inherit variable-pitch
            :foreground ,dark-cyan
            :background ,bg-dark)
           (org-agenda-current-time
            :foreground ,slate)
           (org-indent
            :inherit org-hide)
           (org-time-grid
            :foreground ,comment)
           (org-warning
            :foreground ,builtin)
           (org-agenda-structure
            :height 1.3
            :foreground ,doc
            :weight normal
            :inherit variable-pitch)
           (org-agenda-date
            :foreground ,doc
            :inherit variable-pitch)
           (org-agenda-date-today
            :height 1.5
            :foreground ,keyword
            :inherit variable-pitch)
           (org-agenda-date-weekend
            :inherit org-agenda-date)
           (org-scheduled
            :foreground ,gray)
           (org-upcoming-deadline
            :foreground ,keyword)
           (org-scheduled-today
            :foreground ,fg-white)
           (org-scheduled-previously
            :foreground ,slate)
           (org-agenda-done
            :inherit nil
            :strike-through t
            :foreground ,doc)
           (org-ellipsis
            :underline nil
            :foreground ,comment)
           (org-tag
            :foreground ,doc)
           (org-table
            ;:family ,serif-mono-font
            ;:height 0.9
            :background nil)
           (font-latex-sectioning-0-face
            :foreground ,type
            :height 1.2)
           (font-latex-sectioning-1-face
            :foreground ,type
            :height 1.1)
           (font-latex-sectioning-2-face
            :foreground ,type
            :height 1.1)
           (font-latex-sectioning-3-face
            :foreground ,type
            :height 1.0)
           (font-latex-sectioning-4-face
            :foreground ,type
            :height 1.0)
           (font-latex-sectioning-5-face
            :foreground ,type
            :height 1.0)
           (font-latex-verbatim-face
            :foreground ,builtin)
           (spacemacs-normal-face
            :background ,bg-dark
            :foreground ,fg-white)
           (spacemacs-evilified-face
            :background ,bg-dark
            :foreground ,fg-white)
           (spacemacs-lisp-face
            :background ,bg-dark
            :foreground ,fg-white)
           (spacemacs-emacs-face
            :background ,bg-dark
            :foreground ,fg-white)
           (spacemacs-motion-face
            :background ,bg-dark
            :foreground ,fg-white)
           (spacemacs-visual-face
            :background ,bg-dark
            :foreground ,fg-white)
           (spacemacs-hybrid-face
            :background ,bg-dark
            :foreground ,fg-white)
           (bm-persistent-face
            :background ,dark-cyan
            :foreground ,fg-white)
           (helm-selection
            :background ,region)
           (helm-match
            :foreground ,keyword)
           (cfw:face-title
            :height 2.0
            :inherit variable-pitch
            :weight bold
            :foreground ,doc)
           (cfw:face-holiday
            :foreground ,builtin)
           (cfw:face-saturday
            :foreground ,doc
            :weight bold)
           (cfw:face-sunday
            :foreground ,doc)
           (cfw:face-periods
            :foreground ,dark-cyan)
           (cfw:face-annotation
            :foreground ,doc)
           (cfw:face-select
            :background ,region)
           (cfw:face-toolbar-button-off
            :foreground ,doc)
           (cfw:face-toolbar-button-on
            :foreground ,type
            :weight bold)
           (cfw:face-day-title
            :foreground ,doc)
           (cfw:face-default-content
            :foreground ,dark-cyan)
           (cfw:face-disable
            :foreground ,doc)
           (cfw:face-today
            :background ,region
            :weight bold)
           (cfw:face-toolbar
            :inherit default)
           (cfw:face-today-title
            :background ,keyword
            :foreground ,fg-white)
           (cfw:face-grid
            :foreground ,comment)
           (cfw:face-header
            :foreground ,keyword
            :weight bold)
           (cfw:face-default-day
            :foreground ,fg-white)
           (dired-subtree-depth-1-face
            :background nil)
           (dired-subtree-depth-2-face
            :background nil)
           (dired-subtree-depth-3-face
            :background nil)
           (dired-subtree-depth-4-face
            :background nil)
           (dired-subtree-depth-5-face
            :background nil)
           (dired-subtree-depth-6-face
            :background nil)
           (nlinum-current-line
            :foreground ,builtin)
           (vertical-border
            :background ,region
            :foreground ,region)
           (which-key-command-description-face
            :foreground ,type)
           (flycheck-error
            :background nil)
           (flycheck-warning
            :background nil)
           (font-lock-string-face
            :foreground ,string)
           (font-lock-comment-face
            :foreground ,doc
            :slant italic)
           (helm-ff-symlink
            :foreground ,slate)
           (region
            :background ,region)
           (header-line
            :background nil
            :inherit nil))
          ;; White theme for org
          (spacemacs-light
           (variable-pitch
            :family ,et-font
            :background nil
            :foreground ,bg-dark
            :height 1.7)
           (header-line
            :background nil
            :inherit nil)
           (magit-header-line
            :background nil
            :foreground ,bg-white
            :box nil)
           (org-table
            :family ,serif-mono-font
            :height 0.9
            :background ,bg-white)
           (org-document-title
            :inherit nil
            :family ,et-font
            :height 1.8
            :foreground ,bg-dark
            :underline nil)
           (org-document-info-keyword
            :height 0.8
            :foreground ,gray)
           (org-document-info
            :height 1.2
            :slant italic)
           (org-date
            :family ,sans-mono-font
            :height 0.8)
           (org-special-keyword
            :family ,sans-mono-font
            :height 0.8)
           (org-level-1
            :inherit nil
            :family ,et-font
            :height 1.6
            :weight normal
            :slant normal
            :foreground ,bg-dark)
           (org-level-2
            :inherit nil
            :family ,et-font
            :weight normal
            :height 1.3
            :slant italic
            :foreground ,bg-dark)
           (org-level-3
            :inherit nil
            :family ,et-font
            :weight normal
            :slant italic
            :height 1.2
            :foreground ,bg-dark)
           (org-level-4
            :inherit nil
            :family ,et-font
            :weight normal
            :slant italic
            :height 1.1
            :foreground ,bg-dark)
           (org-headline-done
            :family ,et-font
            :strike-through t)
           (org-agenda-done
            :strike-through t
            :foreground ,doc)
           (org-ellipsis
            :underline nil
            :foreground ,comment)
           (org-tag
            :foreground ,doc)
           (org-hide
            :foreground ,bg-white)
           (org-indent
            :inherit (org-hide fixed-pitch))
           (org-block-end-line
            :background nil
            :height 0.8
            :family ,sans-mono-font
            :foreground ,slate)
           (org-block-begin-line
            :background nil
            :height 0.8
            :family ,sans-mono-font
            :foreground ,slate)
           (org-block
            :background nil
            :foreground ,bg-dark)
           (org-link
            :foreground ,bg-dark)
           (nlinum-current-line
            :foreground ,bg-dark)
           (font-lock-comment-face
            :background nil
            :foreground ,doc
            :slant italic)
           (org-code
            :inherit nil
            :family ,serif-mono-font
            :foreground ,comment
            :height 0.9)
           (org-agenda-date-today
            :inherit variable-pitch
            :height 1.1)
           (mode-line
            :background ,bg-white
            :box nil)
           (mode-line-inactive
            :box nil)
           (highlight
            :background ,shade-white)
           (powerline-active1
            :background ,bg-white)
           (powerline-active2
            :background ,bg-white)
           (powerline-inactive1
            :background ,bg-white)
           (powerline-inactive2
            :background ,bg-white)
           (doom-neotree-dir-face
            :family ,sans-font
            :height 1.0)
           (doom-neotree-file-face
            :family ,sans-font
            :height 1.0)
           (doom-neotree-text-file-face
            :family ,sans-font
            :height 1.0)
           (doom-neotree-hidden-file-face
            :family ,sans-font
            :height 1.0
            :foreground ,comment)
           (doom-neotree-media-file-face
            :family ,sans-font
            :height 1.0
            :foreground ,type)
           (doom-neotree-data-file-face
            :family ,sans-font
            :height 1.0
            :foreground ,doc)))))
