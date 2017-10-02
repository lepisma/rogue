;;; config.el --- rogue Layer config File for Spacemacs
;; Mostly theme configuration

(defvar rogue-dark-theme 'doom-molokai)
(defvar rogue-light-theme 'spacemacs-light)

(defvar rogue-current-theme rogue-light-theme
  "Currently active color scheme")

(defmacro set-pair-faces (themes consts faces-alist)
  "Macro for pair setting of custom faces.
THEMES name the pair (theme-one theme-two). CONSTS sets the variables like
  ((sans-font \"Some Sans Font\") ...). FACES-ALIST has the actual faces
like:
  ((face1 theme-one-attr theme-two-atrr)
   (face2 theme-one-attr nil           )
   (face3 nil            theme-two-attr)
   ...)"
  (defmacro get-proper-faces ()
    `(let* (,@consts)
       (backquote ,faces-alist)))

  `(setq theming-modifications
         ',(mapcar (lambda (theme)
                     `(,theme ,@(cl-remove-if
                                 (lambda (x) (equal x "NA"))
                                 (mapcar (lambda (face)
                                           (let ((face-name (car face))
                                                 (face-attrs (nth (cl-position theme themes) (cdr face))))
                                             (if face-attrs
                                                 `(,face-name ,@face-attrs)
                                               "NA"))) (get-proper-faces)))))
                   themes)))

(set-pair-faces
 ;; Themes to cycle in
 (doom-molokai spacemacs-light)

 ;; Variables
 ((bg-white           "#fbf8ef")
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
  (gray-dark          "#999")
  (gray               "#bbb")
  (sans-font          "Source Sans Pro")
  (serif-font         "Merriweather")
  (et-font            "EtBembo")
  (sans-mono-font     "Souce Code Pro")
  (serif-mono-font    "Verily Serif Mono"))

 ;; Settings
 ((variable-pitch
   (:family ,sans-font)
   (:family ,et-font
            :background nil
            :foreground ,bg-dark
            :height 1.7))
  (header-line
   (:background nil :inherit nil)
   (:background nil :inherit nil))
  (eval-sexp-fu-flash
   (:background ,dark-cyan
                :foreground ,fg-white)
   nil)
  (eval-sexp-fu-flash-error
   (:background ,keyword
                :foreground ,fg-white)
   nil)
  (hackernews-link-face
   (:foreground ,slate
                :inherit variable-pitch
                :height 1.2)
   nil)
  (hackernews-comment-count-face
   (:foreground ,string)
   nil)
  (company-tooltip
   (:background ,bg-darker
                :foreground ,gray)
   nil)
  (company-scrollbar-fg
   (:background ,comment)
   nil)
  (company-scrollbar-bg
   (:background ,bg-darker)
   nil)
  (company-tooltip-common
   (:foreground ,keyword)
   nil)
  (company-tootip-annotation
   (:foreground ,type)
   nil)
  (company-tooltip-selection
   (:background ,region)
   nil)
  (show-paren-match
   (:background ,keyword
                :foreground ,bg-dark)
   nil)
  (magit-section-heading
   (:foreground ,keyword)
   nil)
  (magit-header-line
   (:background nil
                :foreground ,bg-dark
                :box nil)
   (:background nil
                :foreground ,bg-white
                :box nil))
  (magit-diff-hunk-heading
   (:background ,comment
                :foreground ,gray)
   nil)
  (magit-diff-hunk-heading-highlight
   (:background ,comment
                :foreground ,fg-white)
   nil)
  (tooltip
   (:foreground ,gray
                :background ,bg-darker)
   nil)
  (git-gutter-fr:modified
   (:foreground ,dark-cyan)
   nil)
  (doom-neotree-dir-face
   (:foreground ,keyword
                :height 1.0)
   (:family ,sans-font
            :height 1.0))
  (doom-neotree-file-face
   (:height 1.0)
   (:family ,sans-font
            :height 1.0))
  (doom-neotree-text-file-face
   (:height 1.0)
   (:family ,sans-font
            :height 1.0))
  (doom-neotree-hidden-file-face
   (:height 1.0
            :foreground ,comment)
   (:family ,sans-font
            :height 1.0
            :foreground ,comment))
  (doom-neotree-media-file-face
   (:height 1.0
            :foreground ,type)
   (:family ,sans-font
            :height 1.0
            :foreground ,type))
  (doom-neotree-data-file-face
   (:height 1.0
            :foreground ,doc)
   (:family ,sans-font
            :height 1.0
            :foreground ,doc))
  (neo-root-dir-face
   (:foreground ,fg-white
                :background ,region-dark
                :box (:line-width 6 :color ,region-dark))
   nil)
  (mode-line
   (:background ,bg-darker)
   (:background ,bg-white
                :box nil))
  (mode-line-inactive
   nil
   (:box nil))
  (powerline-active1
   nil
   (:background ,bg-white))
  (powerline-active2
   nil
   (:background ,bg-white))
  (powerline-inactive1
   nil
   (:background ,bg-white))
  (powerline-inactive2
   nil
   (:background ,bg-white))
  (highlight
   (:background ,region
                :foreground ,fg-white)
   (:background ,shade-white))
  (hl-line
   (:background ,region-dark)
   nil)
  (solaire-hl-line-face
   (:background ,bg-dark)
   nil)
  (org-document-title
   (:inherit variable-pitch
             :height 1.3
             :weight normal
             :foreground ,gray)
   (:inherit nil
             :family ,et-font
             :height 1.8
             :foreground ,bg-dark
             :underline nil))
  (org-document-info
   (:foreground ,gray
                :slant italic)
   (:height 1.2
            :slant italic))
  (org-level-1
   (:inherit variable-pitch
             :height 1.3
             :weight bold
             :foreground ,keyword
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :height 1.6
             :weight normal
             :slant normal
             :foreground ,bg-dark))
  (org-level-2
   (:inherit variable-pitch
             :weight bold
             :height 1.2
             :foreground ,gray
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :height 1.3
             :slant italic
             :foreground ,bg-dark))
  (org-level-3
   (:inherit variable-pitch
             :weight bold
             :height 1.1
             :foreground ,slate
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :slant italic
             :height 1.2
             :foreground ,bg-dark))
  (org-level-4
   (:inherit variable-pitch
             :weight bold
             :height 1.1
             :foreground ,slate
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :slant italic
             :height 1.1
             :foreground ,bg-dark))
  (org-level-5
   (:inherit variable-pitch
             :weight bold
             :height 1.1
             :foreground ,slate
             :background ,bg-dark)
   nil)
  (org-level-6
   (:inherit variable-pitch
             :weight bold
             :height 1.1
             :foreground ,slate
             :background ,bg-dark)
   nil)
  (org-level-7
   (:inherit variable-pitch
             :weight bold
             :height 1.1
             :foreground ,slate
             :background ,bg-dark)
   nil)
  (org-level-8
   (:inherit variable-pitch
             :weight bold
             :height 1.1
             :foreground ,slate
             :background ,bg-dark)
   nil)
  (org-headline-done
   (:strike-through t)
   (:family ,et-font
            :strike-through t))
  (org-quote
   (:background ,bg-dark)
   nil)
  (org-block
   (:background ,bg-dark)
   (:background nil
                :foreground ,bg-dark))
  (org-block-begin-line
   (:background ,bg-dark)
   (:background nil
                :height 0.8
                :family ,sans-mono-font
                :foreground ,slate))
  (org-block-end-line
   (:background ,bg-dark)
   (:background nil
                :height 0.8
                :family ,sans-mono-font
                :foreground ,slate))
  (org-document-info-keyword
   (:foreground ,comment)
   (:height 0.8
            :foreground ,gray))
  (org-link
   (:underline nil
               :weight normal
               :foreground ,slate)
   (:foreground ,bg-dark))
  (org-special-keyword
   (:height 0.9
            :foreground ,comment)
   (:family ,sans-mono-font
            :height 0.8))
  (org-todo
   (:foreground ,builtin
                :background ,bg-dark)
   nil)
  (org-done
   (:inherit variable-pitch
             :foreground ,dark-cyan
             :background ,bg-dark)
   nil)
  (org-agenda-current-time
   (:foreground ,slate)
   nil)
  (org-hide
   nil
   (:foreground ,bg-white))
  (org-indent
   (:inherit org-hide)
   (:inherit (org-hide fixed-pitch)))
  (org-time-grid
   (:foreground ,comment)
   nil)
  (org-warning
   (:foreground ,builtin)
   nil)
  (org-date
   nil
   (:family ,sans-mono-font
            :height 0.8))
  (org-agenda-structure
   (:height 1.3
            :foreground ,doc
            :weight normal
            :inherit variable-pitch)
   nil)
  (org-agenda-date
   (:foreground ,doc
                :inherit variable-pitch)
   (:inherit variable-pitch
             :height 1.1))
  (org-agenda-date-today
   (:height 1.5
            :foreground ,keyword
            :inherit variable-pitch)
   nil)
  (org-agenda-date-weekend
   (:inherit org-agenda-date)
   nil)
  (org-scheduled
   (:foreground ,gray)
   nil)
  (org-upcoming-deadline
   (:foreground ,keyword)
   nil)
  (org-scheduled-today
   (:foreground ,fg-white)
   nil)
  (org-scheduled-previously
   (:foreground ,slate)
   nil)
  (org-agenda-done
   (:inherit nil
             :strike-through t
             :foreground ,doc)
   (:strike-through t
                    :foreground ,doc))
  (org-ellipsis
   (:underline nil
               :foreground ,comment)
   (:underline nil
               :foreground ,comment))
  (org-tag
   (:foreground ,doc)
   (:foreground ,doc))
  (org-table
   (:background nil)
   (:family ,serif-mono-font
            :height 0.9
            :background ,bg-white))
  (org-code
   (:inherit font-lock-builtin-face)
   (:inherit nil
             :family ,serif-mono-font
             :foreground ,comment
             :height 0.9))
  (font-latex-sectioning-0-face
   (:foreground ,type
                :height 1.2)
   nil)
  (font-latex-sectioning-1-face
   (:foreground ,type
                :height 1.1)
   nil)
  (font-latex-sectioning-2-face
   (:foreground ,type
                :height 1.1)
   nil)
  (font-latex-sectioning-3-face
   (:foreground ,type
                :height 1.0)
   nil)
  (font-latex-sectioning-4-face
   (:foreground ,type
                :height 1.0)
   nil)
  (font-latex-sectioning-5-face
   (:foreground ,type
                :height 1.0)
   nil)
  (font-latex-verbatim-face
   (:foreground ,builtin)
   nil)
  (spacemacs-normal-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (spacemacs-evilified-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (spacemacs-lisp-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (spacemacs-emacs-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (spacemacs-motion-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (spacemacs-visual-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (spacemacs-hybrid-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (bm-persistent-face
   (:background ,dark-cyan
                :foreground ,fg-white)
   nil)
  (helm-selection
   (:background ,region)
   nil)
  (helm-match
   (:foreground ,keyword)
   nil)
  (cfw:face-title
   (:height 2.0
            :inherit variable-pitch
            :weight bold
            :foreground ,doc)
   nil)
  (cfw:face-holiday
   (:foreground ,builtin)
   nil)
  (cfw:face-saturday
   (:foreground ,doc
                :weight bold)
   nil)
  (cfw:face-sunday
   (:foreground ,doc)
   nil)
  (cfw:face-periods
   (:foreground ,dark-cyan)
   nil)
  (cfw:face-annotation
   (:foreground ,doc)
   nil)
  (cfw:face-select
   (:background ,region)
   nil)
  (cfw:face-toolbar-button-off
   (:foreground ,doc)
   nil)
  (cfw:face-toolbar-button-on
   (:foreground ,type
                :weight bold)
   nil)
  (cfw:face-day-title
   (:foreground ,doc)
   nil)
  (cfw:face-default-content
   (:foreground ,dark-cyan)
   nil)
  (cfw:face-disable
   (:foreground ,doc)
   nil)
  (cfw:face-today
   (:background ,region
                :weight bold)
   nil)
  (cfw:face-toolbar
   (:inherit default)
   nil)
  (cfw:face-today-title
   (:background ,keyword
                :foreground ,fg-white)
   nil)
  (cfw:face-grid
   (:foreground ,comment)
   nil)
  (cfw:face-header
   (:foreground ,keyword
                :weight bold)
   nil)
  (cfw:face-default-day
   (:foreground ,fg-white)
   nil)
  (dired-subtree-depth-1-face
   (:background nil)
   nil)
  (dired-subtree-depth-2-face
   (:background nil)
   nil)
  (dired-subtree-depth-3-face
   (:background nil)
   nil)
  (dired-subtree-depth-4-face
   (:background nil)
   nil)
  (dired-subtree-depth-5-face
   (:background nil)
   nil)
  (dired-subtree-depth-6-face
   (:background nil)
   nil)
  (nlinum-current-line
   (:foreground ,builtin)
   (:foreground ,bg-dark))
  (vertical-border
   (:background ,region
                :foreground ,region)
   nil)
  (which-key-command-description-face
   (:foreground ,type)
   nil)
  (flycheck-error
   (:background nil)
   nil)
  (flycheck-warning
   (:background nil)
   nil)
  (font-lock-string-face
   (:foreground ,string)
   nil)
  (font-lock-comment-face
   (:foreground ,doc
                :slant italic)
   (:background nil
                :foreground ,doc
                :slant italic))
  (helm-ff-symlink
   (:foreground ,slate)
   nil)
  (region
   (:background ,region)
   nil)
  (header-line
   (:background nil
                :inherit nil)
   (:background nil
                :inherit nil))))
