;;; config.el --- rogue Layer config File for Spacemacs
;; Mostly theme configuration

(require 'color)

(defvar rogue-dark-theme 'doom-molokai)
(defvar rogue-light-theme 'spacemacs-light)

(defvar rogue-current-theme rogue-dark-theme
  "Currently active color scheme")

(defmacro rogue-set-pair-faces (themes consts faces-alist)
  "Macro for pair setting of custom faces.
THEMES name the pair (theme-one theme-two). CONSTS sets the variables like
  ((sans-font \"Some Sans Font\") ...). FACES-ALIST has the actual faces
like:
  ((face1 theme-one-attr theme-two-atrr)
   (face2 theme-one-attr nil           )
   (face3 nil            theme-two-attr)
   ...)"
  (defmacro rogue--get-proper-faces ()
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
                                               "NA"))) (rogue--get-proper-faces)))))
                   themes)))

(rogue-set-pair-faces
 ;; Themes to cycle in
 (doom-molokai spacemacs-light)

 ;; Variables
 (;; Palette from desktop color scheme
  (dark-1             "#2E3440")
  (dark-2             "#3B4252")
  (dark-3             "#434C5E")
  (dark-4             "#4C566A")
  (light-1            "#D8DEE9")
  (light-2            "#E5E9F0")
  (light-3            "#ECEFF4")
  (accent-dark        "#1C2028")
  (accent-dark-gray   (color-darken-name accent-dark 1))
  (accent-light       "#8a9899")
  (accent-shade-1     "#8FBCBB")
  (accent-shade-2     "#88C0D0")
  (accent-shade-3     "#81A1C1")
  (accent-shade-4     "#5E81AC")
  (colors-blue        accent-shade-4)
  (colors-red         "#BF616A")
  (colors-orange      "#D08770")
  (colors-yellow      "#EBCB8B")
  (colors-green       "#A3BE8C")
  (colors-purple      "#B48EAD")

  ;; For use in levelified faces set
  (level-1            colors-blue)
  (level-2            colors-red)
  (level-3            colors-purple)
  (level-4            colors-orange)
  (level-5            accent-shade-3)
  (level-6            colors-green)
  (level-7            accent-shade-2)
  (level-8            colors-yellow)
  (level-9            accent-shade-1)

  ;; Base gray shades
  (bg-white           "#FEFFF9")
  (bg-dark            accent-dark-gray)
  (bg-darker          accent-dark)
  (bg-dark-solaire    (color-lighten-name accent-dark 2))
  (fg-white           light-3)
  (shade-white        light-1)
  (highlight          (color-lighten-name accent-dark 4))
  (region-dark        (color-lighten-name accent-dark 2))
  (region             dark-3)
  (slate              accent-shade-3)
  (gray               (color-lighten-name dark-4 20))

  ;; Programming
  (comment            (color-lighten-name dark-4 2))
  (doc                (color-lighten-name dark-4 20))
  (keyword            colors-red)
  (builtin            colors-orange)
  (variable-name      colors-yellow)
  (function-name      accent-shade-2)
  (constant           colors-purple)
  (type               accent-shade-1)
  (string             colors-green)

  ;; Fonts
  (sans-font          "Source Sans Pro")
  (et-font            "EtBembo"))

 ;; Settings
 ((variable-pitch
   (:family ,sans-font)
   (:family ,et-font
            :background nil
            :foreground ,bg-dark
            :height 1.0))
  (default
    (:background ,bg-dark)
    (:background ,bg-white))
  (cursor
   (:background ,colors-red)
   nil)
  (which-key-key-face
   (:foreground ,string)
   nil)
  (which-key-command-description-face
   (:foreground ,type)
   nil)
  (which-key-local-map-description-face
   (:foreground ,variable-name)
   nil)
  (solaire-default-face
   (:background ,bg-dark-solaire)
   nil)
  (header-line
   (:background nil :inherit nil)
   (:background nil :inherit nil))
  (eval-sexp-fu-flash
   (:background ,colors-blue
                :foreground ,fg-white)
   nil)
  (eval-sexp-fu-flash-error
   (:background ,keyword
                :foreground ,fg-white)
   nil)
  (hackernews-link
   (:foreground ,slate)
   nil)
  (hackernews-comment-count
   (:foreground ,string)
   nil)
  (company-tooltip
   (:background ,bg-darker
                :foreground ,doc)
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
  (company-tooltip-mouse
   (:background ,accent-shade-3)
   nil)
  (company-tootip-annotation
   (:foreground ,type)
   nil)
  (company-tooltip-selection
   (:background ,highlight)
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
   (:foreground ,colors-blue)
   nil)
  (git-gutter-fr:added
   (:foreground ,string)
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
   (:background ,bg-dark)
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
   (:background ,region-dark
                :foreground ,fg-white)
   (:background ,shade-white))
  (hl-line
   (:background ,region-dark)
   nil)
  (solaire-hl-line-face
   (:background ,region-dark)
   nil)
  (linum
   nil
   (:background ,bg-white))
  (org-document-title
   (:inherit variable-pitch
             :height 1.3
             :weight normal
             :foreground ,gray)
   (:inherit nil
             :family ,et-font
             :height 1.7
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
             :height 1.8
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
             :height 1.6
             :slant italic
             :foreground ,bg-dark))
  (org-level-3
   (:inherit variable-pitch
             :weight bold
             :height 1.1
             :foreground ,gray
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :slant italic
             :height 1.5
             :foreground ,bg-dark))
  (org-level-4
   (:inherit variable-pitch
             :weight bold
             :height 1.1
             :foreground ,gray
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :slant italic
             :height 1.3
             :foreground ,bg-dark))
  (org-level-5
   (:inherit variable-pitch
             :weight bold
             :height 1.1
             :foreground ,gray
             :background ,bg-dark)
   nil)
  (org-level-6
   (:inherit variable-pitch
             :weight bold
             :height 1.1
             :foreground ,gray
             :background ,bg-dark)
   nil)
  (org-level-7
   (:inherit variable-pitch
             :weight bold
             :height 1.1
             :foreground ,gray
             :background ,bg-dark)
   nil)
  (org-level-8
   (:inherit variable-pitch
             :weight bold
             :height 1.1
             :foreground ,gray
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
                :foreground ,slate))
  (org-block-end-line
   (:background ,bg-dark)
   (:background nil
                :height 0.8
                :foreground ,slate))
  (org-document-info-keyword
   (:foreground ,comment)
   (:height 0.8
            :foreground ,gray))
  (link
   (:foreground ,slate)
   nil)
  (org-list-dt
   (:foreground ,function-name)
   nil)
  (org-link
   (:underline nil
               :weight normal
               :foreground ,slate)
   (:foreground ,bg-dark))
  (org-special-keyword
   (:height 0.9
            :foreground ,comment)
   (:height 0.8))
  (org-todo
   (:foreground ,builtin
                :background ,bg-dark)
   nil)
  (org-done
   (:inherit variable-pitch
             :foreground ,colors-blue
             :background ,bg-dark)
   (:strike-through t
                    :family ,et-font))
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
   (:foreground ,doc)
   (:height 0.8))
  (org-agenda-structure
   (:height 1.3
            :foreground ,doc
            :weight normal
            :inherit variable-pitch)
   nil)
  (org-agenda-date
   (:foreground ,doc
                :inherit variable-pitch)
   (:inherit nil))
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
   (:height 1.0
            :strike-through t
            :foreground ,doc))
  (org-ellipsis
   (:underline nil
               :background ,accent-dark-gray
               :foreground ,comment)
   (:underline nil
               :foreground ,comment))
  (org-tag
   (:foreground ,doc)
   (:foreground ,doc))
  (org-table
   (:background nil
                :foreground ,doc)
   (:height 0.9
            :background ,bg-white))
  (org-code
   (:foreground ,builtin)
   (:inherit nil
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
  (minibuffer-prompt
   (:foreground ,keyword)
   nil)
  (swiper-line-face
   (:background ,dark-3
                :foreground ,fg-white)
   nil)
  (swiper-match-face-2
   (:background ,builtin)
   nil)
  (ido-first-match
   (:foreground ,constant)
   nil)
  (helm-M-x-key
   (:foreground ,builtin)
   nil)
  (helm-grep-match
   (:foreground ,constant)
   nil)
  (helm-ff-directory
   (:foreground ,builtin)
   nil)
  (helm-ff-symlink
   (:foreground ,slate)
   nil)
  (helm-ff-dotted-symlink-directory
   (:background nil)
   nil)
  (helm-selection
   (:background ,highlight)
   nil)
  (helm-match
   (:foreground ,keyword)
   nil)
  (helm-ff-prefix
   (:foreground ,keyword)
   nil)
  (helm-separator
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
   (:foreground ,colors-blue)
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
   (:foreground ,colors-blue)
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
  (flycheck-error
   (:background nil)
   nil)
  (flycheck-warning
   (:background nil)
   nil)
  (font-lock-string-face
   (:foreground ,string)
   nil)
  (font-lock-keyword-face
   (:foreground ,keyword)
   nil)
  (font-lock-builtin-face
   (:foreground ,builtin)
   nil)
  (font-lock-type-face
   (:foreground ,type)
   nil)
  (font-lock-doc-face
   (:foreground ,doc)
   nil)
  (font-lock-function-name-face
   (:foreground ,function-name)
   nil)
  (font-lock-variable-name-face
   (:foreground ,variable-name)
   nil)
  (font-lock-constant-face
   (:foreground ,constant)
   nil)
  (font-lock-comment-face
   (:foreground ,doc
                :slant italic)
   (:background nil
                :foreground ,doc
                :slant italic))
  (region
   (:background ,region)
   nil)
  (header-line
   (:background nil
                :inherit nil)
   (:background nil
                :inherit nil))
  (rainbow-delimiters-depth-1-face
   (:foreground ,level-1)
   nil)
  (rainbow-delimiters-depth-2-face
   (:foreground ,level-2)
   nil)
  (rainbow-delimiters-depth-3-face
   (:foreground ,level-3)
   nil)
  (rainbow-delimiters-depth-4-face
   (:foreground ,level-4)
   nil)
  (rainbow-delimiters-depth-5-face
   (:foreground ,level-5)
   nil)
  (rainbow-delimiters-depth-6-face
   (:foreground ,level-6)
   nil)
  (rainbow-delimiters-depth-7-face
   (:foreground ,level-7)
   nil)
  (rainbow-delimiters-depth-8-face
   (:foreground ,level-8)
   nil)
  (rainbow-delimiters-depth-9-face
   (:foreground ,level-9)
   nil)
  (sp-show-pair-match-face
   (:background ,bg-dark)
   nil)
  (sp-pair-overlay-face
   (:background ,bg-dark-solaire)
   nil)
  (sp-wrap-overlay-face
   (:background ,bg-dark-solaire)
   nil)
  (slime-repl-inputed-output-face
   (:foreground ,keyword)
   nil)))

;; Highlight parenthesis
(setq hl-paren-colors '("#88C0D0" "#D08770" "#A3BE8C" "#EBCB8B"))
