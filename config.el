;;; config.el --- rogue Layer config File for Spacemacs

;; Custom theming
(setq theming-modifications
      '((molokai (helm-selection
                  :background "dark slate gray")
                 (helm-source-header
                  :foreground "white"
                  :weight bold
                  :height 1.1)
                 (helm-ff-directory
                  :foreground "dark cyan")
                 (helm-ff-dotted-symlink-directory
                  :foreground "magenta")
                 (helm-ff-symlink
                  :foreground "dark orange")
                 (helm-ff-prefix
                  :background "yellow"
                  :foreground "black")
                 (helm-match
                  :foreground "light sea green"
                  :weight bold)
                 (mode-line-highlight
                  :foreground "gray")
                 (company-scrollbar-bg
                  :background "dark slate gray")
                 (company-scrollbar-fg
                  :background "#1c2c2c")
                 (company-tooltip
                  :background "dark slate gray"
                  :foreground "white")
                 (company-tooltip-annotation
                  :foreground "light salmon")
                 (company-tooltip-selection
                  :background "#4f8e90")
                 (company-preview-common
                  :background "#101A1B"
                  :foreground "white")
                 (vertical-border
                  :background "#1B1D1E"
                  :foreground "#1B1D1E")
                 (shadow
                  :foreground "#566467")
                 (secondary-selection
                  :background "#272822")
                 (fringe
                  :background "#1B1D1E")
                 (highlight
                  :background "#1B1D1E"
                  :foreground "white"
                  :weight bold)
                 (region
                  :background "#111115")
                 (show-paren-match
                  :background "dark cyan"
                  :foreground "white")
                 (show-paren-mismatch
                  :background "purple"
                  :foreground "white")
                 (tooltip
                  :background "#2d4d4d"
                  :foreground "#F8F8F2")
                 (custom-button
                  :background "#101A1B"
                  :foreground "#F8F8F2"
                  :box (:line-width 1 :color "dim gray"))
                 (custom-button-mouse
                  :box (:line-width 1 :color "dim gray")
                  :foreground "#F8F8F2"
                  :background "#2d4d4d")
                 (custom-button-pressed
                  :background "#2d4d4d"
                  :foreground "#F8F8F2"
                  :box (:line-width 2 :color "dim gray" :style pressed-button))
                 (widget-single-line-field
                  :background "#1f2d2c")
                 (widget-field
                  :background "#1f2d2c")
                 (org-level-1
                  :height 1.2
                  :weight bold
                  :foreground "#dddddd"
                  :inherit (variable-pitch))
                 (org-level-2
                  :height 1.2
                  :foreground "#bbbbbb"
                  :inherit (variable-pitch))
                 (org-level-3
                  :height 1.0
                  :weight bold
                  :foreground "#bbbbbb"
                  :inherit (variable-pitch))
                 (org-level-4
                  :height 1.0
                  :foreground "#bbbbbb"
                  :inherit (variable-pitch))
                 (org-done
                  :foreground "lime green"
                  :weight bold)
                 (org-todo
                  :foreground "deep pink"
                  :weight bold)
                 (org-special-keyword
                  :foreground "dark cyan"
                  :weight bold
                  :height 0.9)
                 (org-date
                  :foreground "gold"
                  :underline nil
                  :height 0.9)
                 (org-link
                  :foreground "light sky blue"
                  :underline nil
                  :height 0.9)
                 (org-document-title
                  :inherit (variable-pitch)
                  :height 2.0)
                 (org-priority
                  :foreground "gold"))
        (solarized-light (highlight
                          :background "#fdf6e3")
                         (org-link
                          :slant italic
                          :underline nil)
                         (org-date
                          :underline nil))))
