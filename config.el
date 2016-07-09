;;; config.el --- rogue Layer config File for Spacemacs

;; Enable the usuals
(cua-mode 1)

;; Add line numbers in prog mode
(add-hook 'prog-mode-hook 'linum-mode)

;; Switch to bar
(setq-default cursor-type 'bar)

;; Save desktop
(desktop-save-mode 1)

;; Disable horizontal scroll bar (appears sometimes)
(horizontal-scroll-bar-mode -1)

;; No crappy symbols
(setq dotspacemacs-mode-line-unicode-symbols nil)

;; Add rainbow mode to css and scss
(add-hook 'css-mode-hook (lambda ()
                           (rainbow-mode 1)))

;; Spell check
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Line breaks in text-ish files
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Notes etc.
(setq deft-directory (getenv "NOTES_DIR"))
(setq deft-recursive nil)

(setq org-journal-dir (concat (getenv "NOTES_DIR") "Diary/"))
(setq org-journal-enable-encryption t)

;; Display time in modeline
(display-time-mode 1)

;; Neotree theme
(setq neo-theme 'nerd)

;; Hide vertical border (improves few themes)
(set-face-attribute 'vertical-border
                    nil
                    :foreground (face-attribute
                                 'default
                                 :background))

;; Processing mode setup
(setq processing-location (expand-file-name "~/tools/processing/processing-java"))

;; Clojure symbols
(setq clojure-enable-fancify-symbols t)

;; Set custom region color (for monokai)
(set-face-attribute 'region
                    nil
                    :foreground "white"
                    :background "dark cyan")

(add-hook 'org-mode-hook (lambda ()
                           (setq line-spacing 0.3)))

(eval-after-load "org"
  '(progn
     ;; Org idle time
     (setq org-clock-idle-time 5)

     ;; Org mode symbols
     (setq org-bullets-bullet-list '("•"))

     ;; Modules
     (customize-set-variable 'org-modules '(org-bibtex
                                            org-docview
                                            org-habit
                                            org-info
                                            org-w3m))

     ;; Custom org mode faces
     (customize-set-variable 'org-n-level-faces 4)

     (set-face-attribute 'org-document-title
                         nil
                         :inherit 'variable-pitch
                         :height 1.5
                         :weight 'bold)

     (set-face-attribute 'org-date
                         nil
                         :underline nil)

     (set-face-attribute 'org-level-1
                         nil
                         :inherit 'variable-pitch
                         :height 1.1
                         :weight 'normal
                         :slant 'normal
                         :foreground "turquoise")

     (set-face-attribute 'org-level-2
                         nil
                         :inherit 'variable-pitch
                         :height 1.0
                         :weight 'normal
                         :slant 'normal
                         :foreground "orchid")

     (set-face-attribute 'org-level-3
                         nil
                         :inherit 'variable-pitch
                         :height 1.0
                         :weight 'normal
                         :slant 'normal
                         :foreground "salmon")

     (set-face-attribute 'org-level-4
                         nil
                         :inherit 'variable-pitch
                         :height 1.0
                         :weight 'normal
                         :slant 'normal
                         :foreground "yellow green")))
