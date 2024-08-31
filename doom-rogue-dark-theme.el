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

  ;; name        default   256       16
  ((bg         '("#1D252C" nil       nil            ))
   (bg-alt     '("#181E24" nil       nil            ))
   (base0      '("#10151C" "black"   "black"        ))
   (base1      '("#171D22" "#111122" "brightblack"  ))
   (base2      '("#20282F" "#222222" "brightblack"  ))
   (base3      '("#28323B" "#223333" "brightblack"  ))
   (base4      '("#384551" "#334455" "brightblack"  ))
   (base5      '("#56697A" "#556677" "brightblack"  ))
   (base6      '("#688094" "#668899" "brightblack"  ))
   (base7      '("#7FA0B7" "#77AABB" "brightblack"  ))
   (base8      '("#9CAABB" "#99AABB" "white"        ))
   (fg-alt     '("#728CA0" "#7788AA" "brightwhite"  ))
   (fg         '("#A0B3C5" "#AABBCC" "white"        ))

   (grey        '("#41505E" "#ff6655" "red"          ))
   (red         '("#D95468" "#ff6655" "red"          ))
   (orange      '("#D98E48" "#dd8844" "brightred"    ))
   (green       '("#8BD49C" "#99bb66" "green"        ))
   (teal        '("#33CED8" "#33CCDD" "brightgreen"  ))
   (yellow      '("#EBBF83" "#EEBB88" "yellow"       ))
   (blue        '("#5EC4FF" "#55CCFF" "brightblue"   ))
   (bright-blue '("#539AFC" "#5599FF" "blue"         ))
   (dark-blue   '("#718CA1" "#7788AA" "blue"         ))
   (magenta     '("#E27E8D" "#EE7788" "magenta"      ))
   (violet      '("#B62D65" "#BB2266" "brightmagenta"))
   (cyan        '("#70E1E8" "#77EEEE" "brightcyan"   ))
   (dark-cyan   '("#008B94" "#008899" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.5))
   (selection      dark-blue)
   (builtin        blue)
   (comments       dark-cyan)
   (doc-comments   (doom-lighten dark-cyan 0.25))
   (constants      magenta)
   (functions      teal)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        bright-blue)
   (variables      dark-blue)
   (numbers        magenta)
   (region         base3)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))

   (modeline-fg     'unspecified)
   (modeline-fg-alt base5)
   (modeline-bg base3)
   (modeline-bg-l modeline-bg)
   (modeline-bg-inactive   `(,(car bg) ,@(cdr base1)))
   (modeline-bg-inactive-l (doom-darken bg 0.1)))

  ;;;; Base theme face overrides
  (((font-lock-doc-face &override) :slant 'italic)
   ((font-lock-keyword-face &override) :weight 'bold)
   ((font-lock-builtin-face &override) :weight 'bold)
   ((font-lock-type-face &override) :slant 'italic)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box nil)
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box nil)
   (mode-line-emphasis :foreground base8)
   ;;;; doom-modeline
   (doom-modeline-bar :background modeline-bg)
   ;;;; magit
   (magit-diff-hunk-heading-highlight :foreground fg :background base4 :weight 'bold)
   (magit-diff-hunk-heading :foreground fg-alt :background base3 :weight 'normal)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-url-face    :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override)   :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground blue)
   ((outline-2 &override) :foreground green)
   ((outline-3 &override) :foreground teal)
   ((outline-4 &override) :foreground (doom-darken blue 0.2))
   ((outline-5 &override) :foreground (doom-darken green 0.2))
   ((outline-6 &override) :foreground (doom-darken teal 0.2))
   ((outline-7 &override) :foreground (doom-darken blue 0.4))
   ((outline-8 &override) :foreground (doom-darken green 0.4))
   ;;;; org <built-in>
   ((org-block &override) :background base2)
   ((org-block-begin-line &override) :background base2)
   (org-hide :foreground hidden)
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
   ((treemacs-git-modified-face &override) :family doom-rogue-light-variable-label-face)
   ((treemacs-git-renamed-face &override) :family doom-rogue-light-variable-label-face)
   ((treemacs-git-unmodified-face &override) :family doom-rogue-light-variable-label-face)
   ((treemacs-git-untracked-face &override) :family doom-rogue-light-variable-label-face)
   ((treemacs-tags-face &override) :family doom-rogue-light-variable-label-face :height 0.8)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box nil)
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box nil))

  ;;;; Base theme variable overrides
  ())

(provide 'doom-rogue-dark-theme)

;;; doom-rogue-dark-theme.el ends here
