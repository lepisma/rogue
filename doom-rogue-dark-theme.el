;;; doom-rogue-dark-theme.el --- Personal dark theme forked from doom-plain-dark -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)
;;
;;; Variables

(defgroup doom-rogue-dark-theme nil
  "Options for the `doom-rogue-dark' theme."
  :group 'doom-themes)

(defcustom doom-rogue-brighter-modeline t
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-rogue-dark-theme
  :type 'boolean)

(defcustom doom-rogue-dark-variable-heading-face "Source Sans Pro"
  "Variable pitch font for headings and displays."
  :type 'string
  :group 'doom-rogue-dark-theme)

(defcustom doom-rogue-dark-variable-label-face "Source Sans Pro"
  "Variable pitch font for tags, labels, etc."
  :type 'string
  :group 'doom-rogue-dark-theme)

(defcustom doom-rogue-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-rogue-dark-theme
  :type '(or integer boolean))

;; TODO: Add completions-common-part face

;;
;;; Theme definition

(def-doom-theme doom-rogue-dark
  "Theme modified from doom-plain-dark."
  :family 'doom-rogue
  :background-mode 'dark

  ;; name        default   256       16
  ((bg         '("#222222" nil       nil ))
   (bg-alt     (doom-lighten bg 0.15))
   (base0      '("#838083" nil nil ))
   (base1      '("#0e0c0a" nil nil ))
   (base2      '("#bbbbbb" nil nil ))
   (base3      '("#444444" nil nil ))
   (base4      '("#202020" nil nil ))
   (base5      '("#545053" nil nil ))
   (base6      '("#050505" nil nil ))
   (base7      '("#ffdddd" nil nil ))
   (base8      '("#050505" nil nil ))
   (fg         '("#d7d5d1" nil nil ))
   (fg-alt     '("#e7e5e3" nil nil ))

   (grey       fg)
   (red        fg)
   (blue       fg)
   (dark-blue  fg)
   (orange     fg)
   (green      fg)
   (teal       fg)
   (yellow     fg)
   (magenta    fg)
   (violet     fg)
   (cyan       fg)
   (dark-cyan  fg)

   ;; face categories -- required for all themes
   (highlight      base2)
   (vertical-bar   fg)
   (selection      base1)
   (builtin        base0)
   (comments       base0)
   (doc-comments   base0)
   (constants      base0)
   (functions      fg)
   (keywords       fg)
   (methods        fg)
   (operators      fg)
   (type           fg)
   (strings        base0)
   (variables      base0)
   (numbers        base0)
   (region         base1)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    (doom-darken fg 0.4))
   (vc-added       (doom-lighten fg 0.4))
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-rogue-brighter-modeline)
   (-modeline-pad
    (when doom-rogue-padded-modeline
      (if (integerp doom-rogue-padded-modeline) doom-rogue-padded-modeline 4)))

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg-alt 0.25))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1)))
   (modeline-fg     'unspecified)
   (modeline-fg-alt (doom-lighten modeline-bg-inactive 0.3)))

  ;;;; Base theme face overrides
  (((font-lock-constant-face &override)      :slant 'italic)
   ((font-lock-comment-face &override)       :slant 'italic)
   ((font-lock-function-name-face &override) :slant 'italic)
   ((font-lock-type-face &override)          :slant 'italic)
   (hl-line :background base8)
   ((line-number &override) :foreground base3)
   ((line-number-current-line &override) :foreground base2)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   ;;;; lsp-mode
   (lsp-headerline-breadcrumb-symbols-face :foreground keywords :weight 'bold)
   ;;;; outline <built-in>
   (outline-1 :slant 'italic :foreground fg-alt)
   (outline-2 :inherit 'outline-1 :foreground base2)
   (outline-3 :inherit 'outline-2)
   (outline-4 :inherit 'outline-3)
   (outline-5 :inherit 'outline-4)
   (outline-6 :inherit 'outline-5)
   (outline-7 :inherit 'outline-6)
   (outline-8 :inherit 'outline-7)
   ;;;; org <built-in>
   (org-block-begin-line :foreground base2 :background base3)
   (org-block-end-line :foreground base2 :background base3)
   ((org-document-title &override) :family doom-rogue-dark-variable-heading-face :height 2.5 :foreground fg :weight 'unspecified)
   ((org-level-1 &override) :family doom-rogue-dark-variable-heading-face :height 1.9 :weight 'bold)
   ((org-level-2 &override) :family doom-rogue-dark-variable-heading-face :height 1.6 :weight 'bold)
   ((org-level-3 &override) :family doom-rogue-dark-variable-heading-face :height 1.5 :slant 'italic :weight 'bold)
   ((org-level-4 &override) :family doom-rogue-dark-variable-heading-face :height 1.5 :slant 'italic)
   ((org-level-5 &override) :family doom-rogue-dark-variable-heading-face :height 1.5 :slant 'italic)
   ((org-level-6 &override) :family doom-rogue-dark-variable-heading-face :height 1.5 :slant 'italic)
   ((org-level-7 &override) :family doom-rogue-dark-variable-heading-face :height 1.5 :slant 'italic)
   ((org-level-8 &override) :family doom-rogue-dark-variable-heading-face :height 1.5 :slant 'italic)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))))

;;; doom-rogue-dark-theme.el ends here
