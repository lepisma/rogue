;;; config.el --- rogue Layer config File for Spacemacs
;; Mostly theme configuration

(require 'color)

(defvar r-dark-theme 'doom-molokai)
(defvar r-light-theme 'spacemacs-light)

(defvar r-current-theme r-dark-theme
  "Currently active color scheme")

(defun color-12-to-6 (color-str)
  "Convert 12 char color representation to 6 char"
  (if (= 7 (length color-str))
      color-str
    (apply #'color-rgb-to-hex `(,@(color-name-to-rgb color-str) 2))))

(defmacro r|set-pair-faces (themes consts faces-alist)
  "Macro for pair setting of custom faces.
THEMES name the pair (theme-one theme-two). CONSTS sets the variables like
  ((sans-font \"Some Sans Font\") ...). FACES-ALIST has the actual faces
like:
  ((face1 theme-one-attr theme-two-atrr)
   (face2 theme-one-attr nil           )
   (face3 nil            theme-two-attr)
   ...)
TODO: Simplify this macro"
  (defmacro r||get-proper-faces ()
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
                                               "NA"))) (r||get-proper-faces)))))
                   themes)))

(r|set-pair-faces
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
  (accent-dark-gray   (color-12-to-6 (color-darken-name accent-dark 1)))
  (accent-light       "#8a9899")
  (accent-shade-1     "#8FBCBB")
  (accent-shade-2     "#88C0D0")
  (accent-shade-3     "#81A1C1")
  (accent-shade-4     "#5E81AC")
  (colors-blue        accent-shade-4)
  (colors-blue-2      accent-shade-3)
  (colors-red         "#BF616A")
  (colors-orange      "#8FBCBB")
  (colors-yellow      "#8a9899")
  (colors-green       "#A3BE8C")
  (colors-purple      "#B48EAD")

  ;; For use in levelified faces set
  (level-1            colors-blue)
  (level-2            colors-blue-2)
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
  (bg-dark-solaire    (color-12-to-6 (color-lighten-name accent-dark 2)))
  (fg-white           light-3)
  (shade-white        (color-12-to-6 (color-lighten-name light-1 10)))
  (highlight          (color-12-to-6 (color-lighten-name accent-dark 4)))
  (region-dark        (color-12-to-6 (color-lighten-name accent-dark 50)))
  (region             dark-3)
  (slate              accent-shade-3)
  (gray               (color-12-to-6 (color-lighten-name dark-4 20)))

  ;; Programming
  (comment            (color-12-to-6 (color-lighten-name dark-4 2)))
  (doc                (color-12-to-6 (color-lighten-name dark-4 20)))
  (keyword            colors-blue-2)
  (builtin            colors-orange)
  (variable-name      colors-yellow)
  (function-name      accent-shade-2)
  (constant           colors-purple)
  (type               accent-shade-1)
  (string             colors-green)

  ;; Fonts
  (sans-font          "Source Sans Pro")
  (et-font            "EtBembo")
  (mono-font          "Iosevka"))

 ;; Settings
 ((chronos-notification
   (:foreground ,variable-name
                :inherit variable-pitch
                :height 8.0
                :slant italic)
   nil)
  (chronos-notification-clock
   (:foreground ,comment
                :height 5.0
                :weight bold
                :inherit variable-pitch)
   nil)
  (company-scrollbar-bg
   (:background ,bg-darker)
   nil)
  (company-scrollbar-fg
   (:background ,comment)
   nil)
  (company-tooltip
   (:background ,bg-darker
                :foreground ,doc)
   nil)
  (company-tooltip-common
   (:foreground ,keyword)
   nil)
  (company-tooltip-mouse
   (:background ,accent-shade-3)
   nil)
  (company-tooltip-selection
   (:background ,highlight)
   nil)
  (company-tootip-annotation
   (:foreground ,type)
   nil)
  (coq-cheat-face
   (:inherit font-lock-warning-face)
   (:inherit font-lock-warning-face))
  (coq-context-qualifier-face
   (:inherit font-lock-keyword-face)
   (:inherit font-lock-keyword-face))
  (coq-question-mark-face
   (:inherit font-lock-warning-face)
   (:inherit font-lock-warning-face))
  (coq-solve-tactics-face
   (:inherit font-lock-function-name-face)
   (:inherit font-lock-function-name-face))
  (coq-symbol-binder-face
   (:inherit font-lock-type-face)
   (:inherit font-lock-type-face))
  (coq-symbol-face
   (:inherit font-lock-builtin-face)
   (:inherit font-lock-builtin-face))
  (cursor
   (:background ,colors-purple)
   nil)
  (default
    (:background ,bg-dark)
    (:background ,bg-white))
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
  (doom-modeline-bar
   (:background ,comment)
   nil)
  (doom-modeline-buffer-major-mode
   (:foreground ,comment)
   nil)
  (doom-modeline-buffer-modified
   (:forground ,builtin)
   nil)
  (doom-modeline-buffer-path
   (:foreground ,string)
   nil)
  (doom-modeline-highlight
   (:foreground ,builtin)
   nil)
  (doom-modeline-info
   (:foreground ,variable-name))
  (treemacs-directory-collapsed-face
   (:foreground ,fg-white)
   nil)
  (treemacs-git-added-face
   (:foreground ,colors-green)
   nil)
  (treemacs-git-conflict-face
   (:foreground ,colors-red)
   nil)
  (treemacs-git-modified-face
   (:foreground ,colors-purple)
   nil)
  (treemacs-git-unmodified-face
   (:foreground ,fg-white)
   nil)
  (treemacs-root-face
   (:foreground ,slate
                :height 1.1)
   nil)
  (treemacs-tags-face
   (:foreground ,keyword
                :height 0.9)
   nil)
  (elfeed-search-feed-face
   (:foreground ,slate)
   nil)
  (eval-sexp-fu-flash
   (:background ,colors-blue
                :foreground ,fg-white)
   nil)
  (eval-sexp-fu-flash-error
   (:background ,keyword
                :foreground ,fg-white)
   nil)
  (fixed-pitch
   (:family ,mono-font)
   (:family ,mono-font))
  (flycheck-error
   (:background nil)
   nil)
  (flycheck-warning
   (:background nil)
   nil)
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
  (font-lock-builtin-face
   (:foreground ,builtin)
   nil)
  (font-lock-comment-face
   (:foreground ,doc
                :slant italic)
   (:background nil
                :foreground ,doc
                :slant italic))
  (font-lock-constant-face
   (:foreground ,constant)
   nil)
  (font-lock-doc-face
   (:foreground ,doc)
   nil)
  (font-lock-function-name-face
   (:foreground ,function-name)
   nil)
  (font-lock-keyword-face
   (:foreground ,keyword)
   nil)
  (font-lock-string-face
   (:foreground ,string)
   nil)
  (font-lock-type-face
   (:foreground ,type)
   nil)
  (font-lock-variable-name-face
   (:foreground ,variable-name)
   nil)
  (git-gutter-fr:added
   (:foreground ,string)
   nil)
  (git-gutter-fr:modified
   (:foreground ,colors-blue)
   nil)
  (hackernews-comment-count
   (:foreground ,string)
   nil)
  (hackernews-link
   (:foreground ,slate)
   nil)
  (header-line
   (:background nil :inherit nil)
   (:background nil :inherit nil))
  (helm-M-x-key
   (:foreground ,builtin)
   nil)
  (helm-buffer-file
   (:background nil)
   (:background ,bg-white))
  (helm-ff-directory
   (:foreground ,builtin)
   (:background ,bg-white))
  (helm-ff-dotted-symlink-directory
   (:background nil)
   nil)
  (helm-ff-file
   (:background nil)
   (:background ,bg-white))
  (helm-ff-prefix
   (:foreground ,keyword)
   nil)
  (helm-ff-symlink
   (:foreground ,slate)
   nil)
  (helm-grep-match
   (:foreground ,constant)
   nil)
  (helm-match
   (:foreground ,keyword)
   nil)
  (helm-selection
   (:background ,highlight)
   nil)
  (helm-separator
   (:foreground ,keyword)
   nil)
  (highlight
   (:background ,highlight
                :foreground ,fg-white)
   (:background ,shade-white))
  (highlight-numbers-number
   (:foreground ,constant)
   nil)
  (hl-line
   (:background ,region-dark)
   nil)
  (ido-first-match
   (:foreground ,constant)
   nil)
  (js2-error
   (:foreground nil :inherit font-lock-keyword-face)
   (:foreground nil :inherit font-lock-keyword-face))
  (js2-external-variable
   (:foreground nil :inherit font-lock-variable-name-face)
   (:foreground nil :inherit font-lock-variable-name-face))
  (js2-function-call
   (:foreground nil :inherit font-lock-function-name-face)
   (:foreground nil :inherit font-lock-function-name-face))
  (js2-function-param
   (:foreground nil :inherit font-lock-constant-face)
   (:foreground nil :inherit font-lock-constant-face))
  (js2-instance-member
   (:foreground nil :inherit font-lock-variable-face)
   (:foreground nil :inherit font-lock-variable-face))
  (js2-jsdoc-html-tag-name
   (:foreground nil :inherit font-lock-string-face)
   (:foreground nil :inherit font-lock-string-face))
  (js2-jsdoc-html-tag-delimiter
   (:foreground nil :inherit font-lock-type-face)
   (:foreground nil :inherit font-lock-type-face))
  (js2-jsdoc-tag
   (:foreground nil :inherit font-lock-comment-face)
   (:foreground nil :inherit font-lock-comment-face))
  (js2-jsdoc-type
   (:foreground nil :inherit font-lock-type-face)
   (:foreground nil :inherit font-lock-type-face))
  (js2-jsdoc-value
   (:foreground nil :inherit font-lock-doc-face)
   (:foreground nil :inherit font-lock-doc-face))
  (js2-object-property
   (:foreground nil :inherit font-lock-type-face)
   (:foreground nil :inherit font-lock-type-face))
  (js2-object-property-access
   (:foreground nil :inherit font-lock-type-face)
   (:foreground nil :inherit font-lock-type-face))
  (js2-private-function-call
   (:foreground nil :inherit font-lock-function-name-face)
   (:foreground nil :inherit font-lock-function-name-face))
  (js2-private-member
   (:foreground nil :inherit font-lock-builtin-face)
   (:foreground nil :inherit font-lock-builtin-face))
  (langtool-errline
   (:background nil :inherit flycheck-error)
   (:background nil :inherit flycheck-error))
  (langtool-correction-face
   (:background nil :inherit flycheck-info)
   (:background nil :inherit flycheck-info))
  (line-number-current-line
   (:foreground ,builtin)
   (:foreground ,bg-dark))
  (link
   (:foreground ,slate)
   nil)
  (linum
   (:background nil)
   (:background ,bg-white))
  (lsp-face-highlight-read
   (:background nil
                :foreground nil
                :underline ,colors-blue)
   nil)
  (lsp-face-highlight-textual
   (:background nil
                :foreground nil
                :underline ,colors-blue)
   nil)
  (lsp-face-highlight-write
   (:background nil
                :foreground nil
                :underline ,colors-blue)
   nil)
  (magit-branch-current
   (:foreground ,colors-purple)
   nil)
  (magit-branch-local
   (:foreground ,colors-blue)
   nil)
  (magit-branch-remote
   (:foreground ,colors-green)
   nil)
  (magit-diff-added
   (:background ,(color-12-to-6 (color-darken-name (color-desaturate-name colors-green 20) 50))
                :foreground ,(color-12-to-6 (color-darken-name colors-green 10)))
   nil)
  (magit-diff-added-highlight
   (:background ,(color-12-to-6 (color-darken-name (color-desaturate-name colors-green 20) 45))
                :foreground ,colors-green)
   nil)
  (magit-diff-file-heading-selection
   (:background ,region
                :foreground ,fg-white)
   nil)
  (magit-diff-hunk-heading
   (:background ,region
                :foreground ,gray)
   nil)
  (magit-diff-hunk-heading-highlight
   (:background ,region
                :foreground ,fg-white)
   nil)
  (magit-diff-lines-heading
   (:background ,colors-orange
                :weight bold
                :foreground ,bg-dark)
   nil)
  (magit-diff-removed
   (:background ,(color-12-to-6 (color-darken-name (color-desaturate-name colors-red 40) 40))
                :foreground ,(color-12-to-6 (color-darken-name colors-red 10)))
   nil)
  (magit-diff-removed-highlight
   (:background ,(color-12-to-6 (color-darken-name (color-desaturate-name colors-red 40) 35))
                :foreground ,colors-red)
   nil)
  (magit-header-line
   (:background nil
                :foreground ,bg-dark
                :box nil)
   (:background nil
                :foreground ,bg-white
                :box nil))
  (magit-log-author
   (:foreground ,colors-orange)
   nil)
  (magit-log-date
   (:foreground ,colors-blue)
   nil)
  (magit-section-heading
   (:foreground ,colors-blue-2)
   nil)
  (magit-section-heading-selection
   (:foreground ,colors-yellow)
   nil)
  (markdown-blockquote-face
   (:inherit org-quote :foreground nil)
   (:inherit org-quote :foreground nil))
  (markdown-bold-face
   (:inherit bold :foreground nil)
   (:inherit bold :foreground nil))
  (markdown-code-face
   (:inherit org-code :foreground nil)
   (:inherit org-code :foreground nil))
  (markdown-header-delimiter-face
   (:inherit org-level-1 :foreground ,gray)
   (:inherit org-level-1 :foreground nil))
  (markdown-header-face
   (:inherit org-level-1 :foreground nil)
   (:inherit org-level-1 :foreground nil))
  (markdown-header-face-1
   (:inherit org-level-1 :foreground nil)
   (:inherit org-level-1 :foreground nil))
  (markdown-header-face-2
   (:inherit org-level-2 :foreground nil)
   (:inherit org-level-2 :foreground nil))
  (markdown-header-face-3
   (:inherit org-level-3 :foreground nil)
   (:inherit org-level-3 :foreground nil))
  (markdown-header-face-4
   (:inherit org-level-4 :foreground nil)
   (:inherit org-level-4 :foreground nil))
  (markdown-header-face-5
   (:inherit org-level-5 :foreground nil)
   (:inherit org-level-5 :foreground nil))
  (markdown-header-face-6
   (:inherit org-level-6 :foreground nil)
   (:inherit org-level-6 :foreground nil))
  (markdown-inline-code-face
   (:inherit org-code)
   (:inherit org-code))
  (markdown-italic-face
   (:inherit italic :foreground nil)
   (:inherit italic :foreground nil))
  (markdown-link-face
   (:inherit org-link :foreground nil)
   (:inherit org-link :foreground nil))
  (markdown-list-face
   (:inherit org-list-dt :foreground nil)
   (:inherit org-list-dt :foreground nil))
  (markdown-metadata-key-face
   (:inherit font-lock-keyword-face :foreground nil)
   (:inherit font-lock-keyword-face :foreground nil))
  (markdown-pre-face
   (:inherit org-block :foreground nil)
   (:inherit org-block :foreground nil))
  (markdown-url-face
   (:inherit org-link :foreground nil)
   (:inherit org-link :foreground nil))
  (match
   (:foreground nil
                :background nil
                :underline ,colors-red)
   nil)
  (minibuffer-prompt
   (:foreground ,keyword)
   nil)
  (minimap-active-region-background
   (:background ,region)
   nil)
  (mmm-default-submode-face
   (:background ,bg-dark-solaire)
   nil)
  (mode-line
   (:background ,bg-darker :height 0.9)
   (:background ,bg-white :box nil :height 0.9))
  (mode-line-inactive
   (:background ,bg-dark :height 0.9)
   (:box nil :height 0.9))
  (mu4e-header-highlight-face
   (:underline nil
               :background ,highlight)
   nil)
  (mu4e-header-key-face
   (:foreground ,gray)
   nil)
  (mu4e-header-value-face
   (:foreground ,slate)
   nil)
  (neo-root-dir-face
   (:foreground ,fg-white
                :background ,region-dark
                :box (:line-width 6 :color ,region-dark))
   nil)
  (org-agenda-current-time
   (:foreground ,slate)
   nil)
  (org-agenda-date
   (:foreground ,doc
                :inherit variable-pitch
                :height 1.2)
   (:inherit nil))
  (org-agenda-date-today
   (:height 1.4
            :foreground ,keyword
            :inherit variable-pitch)
   nil)
  (org-agenda-date-weekend
   (:inherit org-agenda-date
             :height 1.0
             :foreground ,comment)
   nil)
  (org-agenda-done
   (:inherit nil
             :strike-through t
             :foreground ,doc)
   (:height 1.0
            :strike-through t
            :foreground ,doc))
  (org-agenda-structure
   (:height 1.3
            :foreground ,doc
            :weight normal
            :inherit variable-pitch)
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
  (org-code
   (:foreground ,builtin
                :height 1.0)
   (:inherit nil
             :foreground ,comment))
  (org-column
   (:background nil
                :weight bold)
   nil)
  (org-column-title
   (:background nil
                :underline t)
   nil)
  (org-date
   (:foreground ,doc)
   (:height 0.8))
  (org-document-info
   (:foreground ,gray
                :slant italic)
   (:height 1.2
            :slant italic))
  (org-document-info-keyword
   (:foreground ,comment)
   (:height 0.8
            :foreground ,gray))
  (org-document-title
   (:inherit variable-pitch
             :height 1.3
             :weight normal
             :foreground ,gray)
   (:inherit nil
             :family ,et-font
             :height 1.4
             :foreground ,bg-dark
             :underline nil))
  (org-done
   (:inherit variable-pitch
             :foreground ,colors-blue
             :background ,bg-dark)
   (:strike-through t
                    :family ,et-font))
  (org-drawer
   (:height 0.9
            :foreground ,comment)
   (:height 0.8))
  (org-ellipsis
   (:underline nil
               :background ,accent-dark-gray
               :foreground ,doc)
   (:underline nil
               :foreground ,comment))
  (org-formula
   (:foreground ,type)
   nil)
  (org-headline-done
   (:strike-through t)
   (:family ,et-font
            :strike-through t))
  (org-hide
   nil
   (:foreground ,bg-white))
  (org-indent
   (:inherit org-hide)
   (:inherit (org-hide fixed-pitch)))
  (org-level-1
   (:inherit variable-pitch
             :height 1.1
             :weight bold
             :foreground ,keyword
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :height 1.4
             :weight normal
             :slant normal
             :foreground ,bg-dark))
  (org-level-2
   (:inherit variable-pitch
             :weight bold
             :height 1.1
             :foreground ,accent-light
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
             :foreground ,gray
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
             :height 1.0
             :foreground ,gray
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :slant italic
             :height 1.2
             :foreground ,bg-dark))
  (org-level-5
   (:inherit variable-pitch
             :weight bold
             :height 1.0
             :foreground ,gray
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :slant italic
             :height 1.2
             :foreground ,bg-dark))
  (org-level-6
   (:inherit variable-pitch
             :weight bold
             :height 1.0
             :foreground ,gray
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :slant italic
             :height 1.2
             :foreground ,bg-dark))
  (org-level-7
   (:inherit variable-pitch
             :weight bold
             :height 1.0
             :foreground ,gray
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :slant italic
             :height 1.2
             :foreground ,bg-dark))
  (org-level-8
   (:inherit variable-pitch
             :weight bold
             :height 1.0
             :foreground ,gray
             :background ,bg-dark)
   (:inherit nil
             :family ,et-font
             :weight normal
             :slant italic
             :height 1.2
             :foreground ,bg-dark))
  (org-link
   (:underline nil
               :weight normal
               :foreground ,slate)
   (:foreground ,bg-dark))
  (org-list-dt
   (:foreground ,function-name)
   nil)
  (org-quote
   (:background ,bg-dark)
   (:slant italic
           :family ,et-font))
  (org-ref-cite-face
   (:foreground ,builtin)
   (:foreground ,builtin))
  (org-ref-ref-face
   (:foreground nil :inherit org-link)
   (:foreground nil :inherit org-link))
  (org-scheduled
   (:foreground ,gray)
   nil)
  (org-scheduled-previously
   (:foreground ,slate)
   nil)
  (org-scheduled-today
   (:foreground ,fg-white)
   nil)
  (org-special-keyword
   (:height 0.9
            :foreground ,comment)
   (:height 0.8))
  (org-table
   (:inherit fixed-pitch
             :background nil
             :foreground ,doc)
   (:inherit fixed-pitch
             :height 0.9
             :background ,bg-white))
  (org-tag
   (:foreground ,doc)
   (:foreground ,doc))
  (org-time-grid
   (:foreground ,comment)
   nil)
  (org-todo
   (:foreground ,builtin
                :background ,bg-dark)
   nil)
  (org-upcoming-deadline
   (:foreground ,keyword)
   nil)
  ;; NOTE: Name is confusing, this is fixed pitch from org-variable-pitch package
  (org-variable-pitch-face
   (:height 0.9)
   nil)
  (org-verbatim
   (:foreground ,type)
   nil)
  (org-warning
   (:foreground ,builtin)
   nil)
  (outline-1
   (:background ,bg-dark-solaire)
   nil)
  (outline-2
   (:background ,bg-dark-solaire)
   nil)
  (outline-3
   (:background ,bg-dark-solaire)
   nil)
  (outline-4
   (:background ,bg-dark-solaire)
   nil)
  (outline-5
   (:background ,bg-dark-solaire)
   nil)
  (outline-6
   (:background ,bg-dark-solaire)
   nil)
  (outline-7
   (:background ,bg-dark-solaire)
   nil)
  (outline-8
   (:background ,bg-dark-solaire)
   nil)
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
  (proof-tactics-name-face
   (:inherit font-lock-constant-face)
   (:inherit font-lock-constant-face))
  (proof-tacticals-name-face
   (:inherit font-lock-variable-face)
   (:inherit font-lock-variable-face))
  (pulse-highlight-start-face
   (:background ,colors-blue :foreground nil)
   (:background ,colors-blue :foreground nil))
  (pulse-highlight-face
   (:background ,colors-blue :foreground nil)
   (:background ,colors-blue :foreground nil))
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
  (region
   (:background ,region)
   nil)
  (secondary-selection
   (:background ,highlight
                :foreground ,fg-white)
   (:background ,shade-white))
  (show-paren-match
   (:background ,keyword
                :foreground ,bg-dark)
   nil)
  (sldb-restartable-frame-line-face
   (:foreground ,colors-green)
   nil)
  (slime-repl-inputed-output-face
   (:foreground ,keyword)
   nil)
  (solaire-default-face
   (:background ,bg-dark-solaire)
   nil)
  (solaire-hl-line-face
   (:background ,region-dark)
   nil)
  (sp-pair-overlay-face
   (:background ,bg-dark-solaire)
   nil)
  (sp-show-pair-match-face
   (:background ,comment
                :foreground ,colors-yellow)
   nil)
  (sp-wrap-overlay-face
   (:background ,bg-dark-solaire)
   nil)
  (spacemacs-emacs-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (spacemacs-evilified-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (spacemacs-hybrid-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (spacemacs-lisp-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (spacemacs-motion-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (spacemacs-normal-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (spacemacs-visual-face
   (:background ,bg-dark
                :foreground ,fg-white)
   nil)
  (swiper-line-face
   (:background ,dark-3
                :foreground ,fg-white)
   nil)
  (swiper-match-face-2
   (:background ,builtin)
   nil)
  (tooltip
   (:foreground ,gray
                :background ,bg-darker)
   nil)
  (variable-pitch
   (:family ,sans-font
            :height 1.1)
   (:family ,et-font
            :background nil
            :foreground ,bg-dark
            :height 1.0))
  (vertical-border
   (:background ,region
                :foreground ,region)
   nil)
  (which-key-command-description-face
   (:foreground ,type)
   nil)
  (which-key-key-face
   (:foreground ,string)
   nil)))

(with-eval-after-load 'highlight-parentheses
  ;; Parentheses colors
  (setq hl-paren-colors '("#88C0D0" "#D08770" "#A3BE8C" "#EBCB8B")))
