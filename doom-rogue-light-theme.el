;;; doom-rogue-light-theme.el --- Light theme inspired by my website's -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-rogue-light-theme nil
  "Options for the `doom-rogue-light' theme."
  :group 'doom-themes)

(defcustom doom-rogue-light-variable-heading-face "EtBembo"
  "Variable pitch font for headings and displays."
  :type 'string
  :group 'doom-rogue-light-theme)

(defcustom doom-rogue-light-variable-body-face "Merriweather"
  "Variable pitch font for regular text body."
  :type 'string
  :group 'doom-rogue-light-theme)

(defcustom doom-rogue-light-variable-label-face "Source Sans Pro"
  "Variable pitch font for tags, labels, etc."
  :type 'string
  :group 'doom-rogue-light-theme)

;;
;;; Theme definition

(def-doom-theme doom-rogue-light
  "Light theme inspired by my website's"

  ;; name        default   256       16
  ((bg         '("#fafafa" "white"   "white"        ))
   (fg         '("#383a42" "#424242" "black"        ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#f0f0f0" "white"   "white"        ))
   (fg-alt     '("#c6c7c7" "#c7c7c7" "brightblack"  ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#f0f0f0" "#f0f0f0" "white"        ))
   (base1      '("#e7e7e7" "#e7e7e7" "brightblack"  ))
   (base2      '("#dfdfdf" "#dfdfdf" "brightblack"  ))
   (base3      '("#c6c7c7" "#c6c7c7" "brightblack"  ))
   (base4      '("#9ca0a4" "#9ca0a4" "brightblack"  ))
   (base5      '("#383a42" "#424242" "brightblack"  ))
   (base6      '("#202328" "#2e2e2e" "brightblack"  ))
   (base7      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base8      '("#1b2229" "black"   "black"        ))

   ;; Core palette
   (primary          '("#0184bc" "#0184bc" "brightcyan"   ))
   (primary-dark     '("#005478" "#005478" "cyan"         ))
   (secondary        '("#6a1868" "#9ca0a4" "brightblack"  ))
   (secondary-dark   '("#202328" "#2e2e2e" "brightblack"  ))

   (grey       base4)
   (red        '("#e45649" "#e45649" "red"          ))
   (orange     '("#da8548" "#dd8844" "brightred"    ))
   (green      '("#50a14f" "#50a14f" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#986801" "#986801" "yellow"       ))
   (blue       '("#4078f2" "#4078f2" "brightblue"   ))
   (dark-blue  '("#a0bcf8" "#a0bcf8" "blue"         ))
   (magenta    '("#a626a4" "#a626a4" "magenta"      ))
   (violet     '("#b751b6" "#b751b6" "brightmagenta"))
   (cyan       '("#0184bc" "#0184bc" "brightcyan"   ))
   (dark-cyan  '("#005478" "#005478" "cyan"         ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      base5)
   (vertical-bar   (doom-darken base2 0.1))
   (selection      dark-blue)
   (builtin        primary-dark)
   (comments       base4)
   (doc-comments   (doom-darken comments 0.15))
   (constants      secondary-dark)
   (functions      dark-cyan)
   (keywords       base8)
   (methods        cyan)
   (operators      blue)
   (type           primary)
   (strings        (doom-darken doc-comments 0.15))
   (variables      (doom-darken secondary 0.36))
   (numbers        orange)
   (region         `(,(doom-darken (car bg-alt) 0.1) ,@(doom-darken (cdr base0) 0.3)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          base4)
   (modeline-bg              (doom-darken base2 0.05))
   (modeline-bg-alt          (doom-darken base2 0.1))
   (modeline-bg-inactive     (doom-darken bg 0.1))
   (modeline-bg-alt-inactive `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base1))))

  ;;;; Base theme face overrides
  (((font-lock-doc-face &override) :slant 'italic)
   ((font-lock-keyword-face &override) :weight 'bold)
   ((font-lock-builtin-face &override) :weight 'bold)
   ((font-lock-type-face &override) :slant 'italic)
   ((line-number &override) :foreground (doom-lighten base4 0.15))
   ((line-number-current-line &override) :foreground base8)
   ((rainbow-delimiters-depth-1-face &override) :foreground primary-dark)
   ((rainbow-delimiters-depth-2-face &override) :foreground secondary-dark)
   ((rainbow-delimiters-depth-3-face &override) :foreground primary-dark)
   ((rainbow-delimiters-depth-4-face &override) :foreground secondary-dark)
   ((rainbow-delimiters-depth-5-face &override) :foreground primary-dark)
   ((rainbow-delimiters-depth-6-face &override) :foreground secondary-dark)
   ((rainbow-delimiters-depth-7-face &override) :foreground primary-dark)
   ((rainbow-delimiters-depth-8-face &override) :foreground secondary-dark)
   ((rainbow-delimiters-depth-9-face &override) :foreground primary-dark)
   (show-paren-match :inherit 'highlight)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box nil)
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box nil)
   (mode-line-emphasis :foreground base8)
   (shadow :foreground base4)
   (tooltip :background base1 :foreground fg)
   ((link &override) :foreground fg :weight 'normal)
   ;;;; doom-modeline
   (doom-modeline-bar :background modeline-bg)
   ;;;; ediff <built-in>
   (ediff-current-diff-A        :foreground red   :background (doom-lighten red 0.8))
   (ediff-current-diff-B        :foreground green :background (doom-lighten green 0.8))
   (ediff-current-diff-C        :foreground blue  :background (doom-lighten blue 0.8))
   (ediff-current-diff-Ancestor :foreground teal  :background (doom-lighten teal 0.8))
   ;;;; lsp-mode
   (lsp-ui-doc-background      :background base0)
   ;;;; magit
   (magit-blame-heading     :foreground orange :background bg-alt)
   ((magit-section-heading &override)   :foreground primary)
   (magit-diff-removed :foreground (doom-darken red 0.2) :background (doom-blend red bg 0.1))
   (magit-diff-removed-highlight :foreground red :background (doom-blend red bg 0.2) :bold bold)
   ;;;; markdown-mode
   (markdown-markup-face     :foreground base5)
   (markdown-header-face     :inherit 'bold :foreground red)
   ((markdown-code-face &override)       :background base1)
   (mmm-default-submode-face :background base1)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground red)
   ((outline-2 &override) :foreground orange)
   ;;;; org <built-in>
   ((org-block &override) :background bg :extend nil)
   ((org-block-begin-line &override) :foreground fg :slant 'italic)
   ((org-code &override) :foreground primary-dark)
   (org-ellipsis :underline nil :background bg     :foreground red)
   ((org-quote &override) :background base1)
   ((org-document-title &override) :family doom-rogue-light-variable-heading-face :height 2.5 :foreground fg :weight 'unspecified)
   ((org-level-1 &override) :family doom-rogue-light-variable-heading-face :height 1.9)
   ((org-level-2 &override) :family doom-rogue-light-variable-heading-face :height 1.6)
   ((org-level-3 &override) :family doom-rogue-light-variable-heading-face :height 1.5 :slant 'italic)
   ((org-level-4 &override) :family doom-rogue-light-variable-heading-face :height 1.5 :slant 'italic)
   ((org-level-5 &override) :family doom-rogue-light-variable-heading-face :height 1.5 :slant 'italic)
   ((org-level-6 &override) :family doom-rogue-light-variable-heading-face :height 1.5 :slant 'italic)
   ((org-level-7 &override) :family doom-rogue-light-variable-heading-face :height 1.5 :slant 'italic)
   ((org-level-8 &override) :family doom-rogue-light-variable-heading-face :height 1.5 :slant 'italic)
   ((org-todo &override) :foreground primary :weight 'bold)
   (org-headline-done :strike-through t :foreground grey)
   (org-table :foreground fg)
   (org-date :foreground secondary)
   ;;;; treemacs
   ((treemacs-async-loading-face &override) :family doom-rogue-light-variable-label-face)
   ((treemacs-directory-collapsed-face &override) :family doom-rogue-light-variable-label-face)
   ((treemacs-directory-face &override) :family doom-rogue-light-variable-label-face)
   ((treemacs-file-face &override) :family doom-rogue-light-variable-label-face)
   ((treemacs-fringe-indicator-face &override) :family doom-rogue-light-variable-label-face)
   ((treemacs-root-face &override) :family doom-rogue-light-variable-label-face :weight 'bold :foreground fg)
   ((treemacs-git-added-face &override) :family doom-rogue-light-variable-label-face)
   ((treemacs-git-conflict-face &override) :family doom-rogue-light-variable-label-face)
   ((treemacs-git-ignored-face &override) :family doom-rogue-light-variable-label-face)
   ((treemacs-git-modified-face &override) :family doom-rogue-light-variable-label-face :foreground primary)
   ((treemacs-git-renamed-face &override) :family doom-rogue-light-variable-label-face)
   ((treemacs-git-unmodified-face &override) :family doom-rogue-light-variable-label-face)
   ((treemacs-git-untracked-face &override) :family doom-rogue-light-variable-label-face)
   ((treemacs-tags-face &override) :family doom-rogue-light-variable-label-face :height 0.8 :foreground primary-dark)
   ;;;; vertico
   (vertico-current :background base2)
   ;;;; wgrep <built-in>
   (wgrep-face :background base1)
   ;;;; whitespace
   ((whitespace-tab &override)         :background (if (not (default-value 'indent-tabs-mode)) base0 'unspecified))
   ((whitespace-indentation &override) :background (if (default-value 'indent-tabs-mode) base0 'unspecified))
   ;;;; eros
   (eros-result-overlay-face :background bg-alt :foreground fg :box `(:line-width -1 :color ,fg-alt))
   ((indent-guide-face &override) :foreground base4 :slant 'normal))
  ;;;; Base theme variable overrides
  ())

(provide 'doom-rogue-light-theme)

;;; doom-rogue-light-theme.el ends here
