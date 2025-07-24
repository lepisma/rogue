;;; doom-rogue-dark-theme.el --- Personal dark theme -*- lexical-binding: t; no-byte-compile: t; -*-

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

(defcustom doom-rogue-dark-variable-label-face "Source Sans Pro"
  "Variable pitch font for tags, labels, etc."
  :type 'string
  :group 'doom-rogue-dark-theme)

;;
;;; Theme definition

(def-doom-theme doom-rogue-dark
  "A personal dark theme"

  ;; name        default      256          16
  ((bg         '("#1B2028" nil          nil           )) ; Main background
   (bg-alt     '("#0D141A" nil          nil           )) ; Darker alternative background
   (base0      '("#0A0F14" "black"      "black"       )) ; Deepest black
   (base1      '("#13181F" "#0A0F14"    "brightblack" )) ; Very dark grey
   (base2      '("#1B2028" "#1B2028"    "brightblack" )) ; Dark grey, often for blocks/borders
   (base3      '("#232A32" "#232A32"    "brightblack" )) ; Medium dark grey
   (base4      '("#2B343C" "#2B343C"    "brightblack" )) ; Slightly lighter dark grey (for highlights)
   (base5      '("#3D4750" "#3D4750"    "brightblack" )) ; Medium grey
   (base6      '("#525D68" "#525D68"    "brightblack" )) ; Lighter grey
   (base7      '("#6B7684" "#6B7684"    "brightblack" )) ; Even lighter grey (for comments)
   (base8      '("#86909D" "#86909D"    "white"       )) ; Light grey
   (fg-alt     '("#A0A8B4" "#A0A8B4"    "brightwhite" )) ; Alternative foreground, subtle
   (fg         '("#CCD4DE" "#BCC4CE"    "white"       )) ; Main foreground, bright and readable

   ;; Core palette - simplified for a more minimal feel, less saturated where possible
   (primary          '("#3A6A99" "#3A6A99" "brightblue"  )) ; Main accent blue
   (secondary        '("#2F5A83" "#2F5A83" "brightblack" )) ; Muted blue

   (grey       base4)
   (red              '("#D95468" "#ff6655" "red"           )) ; Error/deletion color
   (orange           '("#D98E48" "#dd8844" "brightred"     )) ; Warning/modified color, also for numbers
   (green            '("#8BD49C" "#99bb66" "green"         )) ; Success/addition color
   (teal             '("#33CED8" "#33CCDD" "brightgreen"   )) ; Vibrant teal, good for links/some headings
   (yellow           '("#EBBF83" "#EEBB88" "yellow"        )) ; Yellow, for strings
   (blue             '("#5EC4FF" "#55CCFF" "brightblue"    )) ; Bright blue, for operators
   (dark-blue        '("#718CA1" "#7788AA" "blue"          ))
   (magenta          '("#E27E8D" "#EE7788" "magenta"       )) ; Magenta, for constants
   (violet           '("#B62D65" "#BB2266" "brightmagenta" )) ; Deeper violet
   (cyan             '("#70E1E8" "#77EEEE" "brightcyan"    )) ; Bright cyan, for methods
   (dark-cyan        '("#008B94" "#008899" "cyan"          )) ; Muted cyan, for functions

   ;; face categories -- required for all themes
   (highlight     base4) ; General highlight background
   (vertical-bar  (doom-darken base1 0.5)) ; Vertical bar color
   (selection     base3) ; Selection background, darker for better contrast
   (builtin       primary) ; Use primary for built-in functions/types
   (comments      base7) ; Comments, subtle and readable
   (doc-comments  (doom-lighten fg-alt 0.25)) ; Doc comments, slightly brighter than regular comments
   (constants     magenta) ; Constants (e.g., true, false, nil)
   (functions     dark-cyan) ; Function names
   (keywords      primary) ; Keywords (e.g., defun, let)
   (methods       cyan) ; Method names
   (operators     blue) ; Operators (+, -, =, etc.)
   (type          primary) ; Type names
   (strings       primary) ; Strings, vibrant for easy identification
   (variables     primary) ; Variable names
   (numbers       orange) ; Numbers, distinct from constants
   (region        base3) ; Region selection, clear and visible
   (error         red) ; Error messages/highlights
   (warning       orange) ; Warning messages/highlights
   (success       green) ; Success messages/highlights
   (vc-modified   orange) ; Version control modified files
   (vc-added      green) ; Version control added files
   (vc-deleted    red) ; Version control deleted files

   ;; custom categories
   (hidden        `(,(car bg) "black" "black")) ; For hidden text, blends with background

   ;; Modeline colors for active and inactive states
   (modeline-fg          fg) ; Foreground for active modeline
   (modeline-fg-alt      base6) ; Alternative foreground for inactive modeline
   (modeline-bg          base3) ; Background for active modeline
   (modeline-bg-l        (doom-lighten base3 0.05)) ; Slightly lighter background for active modeline
   (modeline-bg-inactive `(,(car bg-alt) ,@(cdr base1))) ; Background for inactive modeline, darker
   (modeline-bg-inactive-l (doom-darken bg-alt 0.1))) ; Slightly darker background for inactive modeline

  ;;;; Base theme face overrides - applying specific styles
  (((font-lock-doc-face &override) :slant 'italic) ; Italic for documentation
   ((font-lock-keyword-face &override) :weight 'bold) ; Bold for keywords
   ((font-lock-builtin-face &override) :weight 'bold) ; Bold for built-in functions
   ((font-lock-type-face &override) :slant 'italic) ; Italic for type names
   ((line-number &override) :foreground (doom-lighten base6 0.1)) ; Subtle line numbers
   ((line-number-current-line &override) :foreground fg :weight 'bold) ; Current line number, bright and bold
   ((rainbow-delimiters-depth-1-face &override) :foreground primary)
   ((rainbow-delimiters-depth-2-face &override) :foreground secondary)
   ((rainbow-delimiters-depth-3-face &override) :foreground primary)
   ((rainbow-delimiters-depth-4-face &override) :foreground secondary)
   ((rainbow-delimiters-depth-5-face &override) :foreground primary)
   ((rainbow-delimiters-depth-6-face &override) :foreground secondary)
   ((rainbow-delimiters-depth-7-face &override) :foreground primary)
   ((rainbow-delimiters-depth-8-face &override) :foreground secondary)
   ((rainbow-delimiters-depth-9-face &override) :foreground primary)
   (show-paren-match :inherit 'highlight) ; Matching parentheses, inherits highlight for minimalism
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box nil)
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box nil)
   (mode-line-emphasis :foreground base8) ; Emphasized text in modeline
   (hl-line :background highlight) ; Highlight current line background
   ((link &override) :foreground blue :weight 'normal :underline t) ; Links, distinct color and underline
   ;;;; doom-modeline
   (doom-modeline-bar :background modeline-bg) ; Modeline bar color
   ;;;; magit
   (magit-diff-hunk-heading-highlight :foreground fg :background base4 :weight 'bold)
   (magit-diff-hunk-heading :foreground fg-alt :background base3 :weight 'normal)
   (magit-diff-added :foreground green) ; Magit diff added lines
   (magit-diff-removed :foreground red) ; Magit diff removed lines
   (magit-diff-context :foreground fg-alt) ; Magit diff context
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5) ; Markdown markup (e.g., **, ##)
   (markdown-header-face :inherit 'bold :foreground primary) ; Markdown headers, bold and distinct
   (markdown-url-face    :foreground blue :weight 'normal :underline t) ; Markdown URLs
   (markdown-reference-face :foreground base6) ; Markdown references
   ((markdown-bold-face &override)   :foreground fg :weight 'bold) ; Bold markdown text
   ((markdown-italic-face &override) :foreground fg-alt :slant 'italic) ; Italic markdown text
   ;;;; outline <built-in> - distinct colors for outline levels, simplified
   ((outline-1 &override) :foreground primary :weight 'bold)
   ((outline-2 &override) :foreground green :weight 'bold)
   ((outline-3 &override) :foreground teal :slant 'italic)
   ((outline-4 &override) :foreground fg-alt :slant 'italic)
   ((outline-5 &override) :foreground fg-alt :slant 'italic)
   ((outline-6 &override) :foreground fg-alt :slant 'italic)
   ((outline-7 &override) :foreground fg-alt :slant 'italic)
   ((outline-8 &override) :foreground fg-alt :slant 'italic)
   ;;;; org <built-in> - improved visibility for Org mode elements
   ((org-block &override) :background base2 :foreground fg-alt) ; Org code blocks, clear background
   ((org-block-begin-line &override) :background base2 :foreground base7 :box `(:line-width 1 :color ,base3 :style nil)) ; Org block delimiters
   ((org-block-end-line &override) :background base2 :foreground base7 :box `(:line-width 1 :color ,base3 :style nil))
   (org-hide :foreground hidden) ; Hidden org text
   ((org-link &override) :foreground blue :underline t) ; Org links, distinct and underlined
   ((org-document-title &override) :family doom-rogue-dark-variable-label-face :height 2.5 :foreground fg :weight 'bold) ; Document title
   ((org-level-1 &override) :family doom-rogue-dark-variable-label-face :height 1.9 :foreground fg :weight 'bold)
   ((org-level-2 &override) :family doom-rogue-dark-variable-label-face :height 1.6 :foreground blue :weight 'bold)
   ((org-level-3 &override) :family doom-rogue-dark-variable-label-face :height 1.5 :foreground teal :slant 'italic)
   ((org-level-4 &override) :family doom-rogue-dark-variable-label-face :height 1.5 :foreground fg-alt :slant 'italic)
   ((org-level-5 &override) :family doom-rogue-dark-variable-label-face :height 1.5 :foreground fg-alt :slant 'italic)
   ((org-level-6 &override) :family doom-rogue-dark-variable-label-face :height 1.5 :foreground fg-alt :slant 'italic)
   ((org-level-7 &override) :family doom-rogue-dark-variable-label-face :height 1.5 :foreground fg-alt :slant 'italic)
   ((org-level-8 &override) :family doom-rogue-dark-variable-label-face :height 1.5 :foreground fg-alt :slant 'italic)
   (org-code :foreground yellow) ; Org code snippets, consistent with strings
   (org-verbatim :foreground yellow) ; Org verbatim text, consistent with strings
   (org-meta-line :foreground base7 :slant 'italic) ; Org meta lines
   (org-footnote :foreground base7 :slant 'italic) ; Org footnotes
   (org-date :foreground base6) ; Org dates
   (org-tag :foreground primary :weight 'bold) ; Org tags
   (org-todo :foreground orange :weight 'bold) ; Org TODO keywords, less alarming
   (org-done :strike-through t :foreground base6) ; Org DONE keywords, struck through and subtle
   (org-ellipsis :underline nil :background bg :foreground (doom-lighten base7 0.2)) ; Org ellipsis, subtle
   ;;;; treemacs
   ((treemacs-async-loading-face &override) :family doom-rogue-dark-variable-label-face :foreground base7)
   ((treemacs-directory-collapsed-face &override) :family doom-rogue-dark-variable-label-face :foreground primary)
   ((treemacs-directory-face &override) :family doom-rogue-dark-variable-label-face :foreground primary)
   ((treemacs-file-face &override) :family doom-rogue-dark-variable-label-face :foreground fg-alt)
   ((treemacs-fringe-indicator-face &override) :family doom-rogue-dark-variable-label-face :foreground base5)
   ((treemacs-root-face &override) :family doom-rogue-dark-variable-label-face :weight 'bold :foreground fg)
   ((treemacs-git-added-face &override) :family doom-rogue-dark-variable-label-face :foreground green)
   ((treemacs-git-conflict-face &override) :family doom-rogue-dark-variable-label-face :foreground red)
   ((treemacs-git-ignored-face &override) :family doom-rogue-dark-variable-label-face :foreground base6)
   ((treemacs-git-modified-face &override) :family doom-rogue-dark-variable-label-face :foreground orange) ; Consistent with light theme
   ((treemacs-git-renamed-face &override) :family doom-rogue-dark-variable-label-face :foreground blue)
   ((treemacs-git-unmodified-face &override) :family doom-rogue-dark-variable-label-face :foreground fg-alt)
   ((treemacs-git-untracked-face &override) :family doom-rogue-dark-variable-label-face :foreground magenta)
   ((treemacs-tags-face &override) :family doom-rogue-dark-variable-label-face :height 0.8 :foreground fg-alt)
   ;;;; nano-vertico - improved contrast for completion UI
   (nano-vertico-header-face :foreground fg :background bg :box `(:line-width 1 :color ,fg-alt :style nil))
   (nano-vertico-mode-line-face :foreground fg-alt :overline nil) ; Removed overline for minimalism
   (nano-vertico-buffer-face :foreground fg-alt :background bg)
   (nano-vertico-prompt-face :foreground bg :background fg-alt :weight 'bold) ; Prompt, reversed contrast
   (nano-vertico-region-face :background base2 :foreground fg :weight 'bold) ; Highlighted candidate, clear and bold
   (nano-vertico-annotation-face :foreground base7 :slant 'italic) ; Annotations, subtle
   (nano-vertico-cursor-face :foreground bg :background primary :weight 'bold) ; Cursor, highly visible
   (cursor :foreground fg) ; Regular cursor
   ;;;; solaire-mode - ensuring consistent background
   (solaire-default-face :background bg) ; Default background for solaire-mode
   (solaire-fringe-face :background bg) ; Fringe background for solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box nil)
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box nil)
   ;;;; eros
   (eros-result-overlay-face :background base2 :foreground fg :box `(:line-width -1 :color ,fg-alt))
   ;;;; indent-guide
   ((indent-guide-face &override) :foreground base4 :slant 'normal) ; Indent guides, subtle but visible
   ;;;; flycheck-overlay - consistent with light theme's approach
   (flycheck-overlay-error :background (doom-darken red 0.6)
                           :foreground fg
                           :height 0.9
                           :weight 'normal)
   (flycheck-overlay-warning :background (doom-darken orange 0.6)
                             :foreground fg
                             :height 0.9
                             :weight 'normal)
   (flycheck-overlay-info :background (doom-darken green 0.6)
                          :foreground fg
                          :height 0.9
                          :weight 'normal))
  ;;;; Base theme variable overrides
  ((flycheck-overlay-info-icon "")
   (flycheck-overlay-warning-icon "")
   (flycheck-overlay-error-icon "")
   (flycheck-overlay-icon-left-padding 0.4)))

(provide 'doom-rogue-dark-theme)

;;; doom-rogue-dark-theme.el ends here
