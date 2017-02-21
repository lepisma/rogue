;;; config.el --- rogue Layer config File for Spacemacs

(defface cursive
  '((t :family "Lora"
       :slant italic))
  "Cursive for org in light mode")

;; TODO: Create a common palette
;; (setq palette-dark
;;       '((white . "#fff")
;;         (org-1 . "#fff")
;;         (org-2 . "#0ee")
;;         (org-3 . "#8470ff")
;;         (org-2 . "#0ee")))

;; Custom theming
(setq theming-modifications
      '((molokai
         (helm-selection
          :background "gray9"
          :weight bold)
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
          :foreground "white"
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
         (company-tooltip-common
          :inherit company-tooltip
          :weight bold)
         (company-tooltip-common-selection
          :inherit company-tooltip-selection
          :weight bold)
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
         (org-document-title
          :inherit (variable-pitch)
          :height 1.5)
         (org-level-1
          :foreground "#fff"
          :inherit (variable-pitch)
          :height 1.0)
         (org-level-2
          :foreground "#0ee"
          :inherit (variable-pitch)
          :height 1.0)
         (org-level-3
          :foreground "#8470ff"
          :inherit (variable-pitch)
          :height 1.0)
         (org-level-4
          :foreground "#0ee"
          :inherit (variable-pitch)
          :height 1.0)
         (org-done
          :foreground "lime green"
          :weight bold)
         (org-todo
          :foreground "#EDEA57"
          :weight bold)
         (org-special-keyword
          :foreground "dark cyan"
          :weight bold)
         (org-scheduled-previously
          :foreground "#0ee")
         (org-scheduled-today
          :foreground "MediumSpringGreen"
          :weight bold)
         (org-date
          :foreground "#EDC27D"
          :underline nil)
         (org-link
          :foreground "light sky blue"
          :underline nil)
         (org-priority
          :foreground "gold"))
        ;; Light theme
        (solarized-light
         (highlight
          :background "#fdf6e3")
         (org-link
          :slant italic
          :underline nil)
         (org-date
          :underline nil)
         (mmm-default-submode-face
          :background "#fdf6e3")
         (org-document-title
          :inherit (cursive)
          :height 1.5)
         (org-level-1
          :inherit (cursive)
          :height 1.3)
         (org-level-2
          :inherit (cursive)
          :height 1.2)
         (org-level-3
          :inherit (cursive)
          :height 1.1)
         (org-level-4
          :inherit (cursive)
          :height 1.1))))
