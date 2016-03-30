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

;; Set notes directory for deft
(setq deft-directory (getenv "NOTES_DIR"))
(setq deft-recursive t)

;; Display time in modeline
(display-time-mode 1)

;; Neotree theme
(setq neo-theme 'nerd)

;; Hide vertical border (improves few themes)
(set-face-attribute 'vertical-border
                    nil
                    :foreground (face-attribute 'default :background))

;; Set custom region color (for monokai)
(set-face-attribute 'region
                    nil
                    :foreground "white"
                    :background "dark cyan")

(eval-after-load "org"
  '(progn
     ;; Org idle time
     (setq org-clock-idle-time 5)

     ;; Org mode symbols
     (setq org-bullets-bullet-list '("◉" "○"))

     ;; Custom org mode faces
     (customize-set-variable 'org-n-level-faces 4)
     (set-face-attribute 'org-default
                         nil
                         :inherit nil
                         :slant 'italic)
     (set-face-attribute 'org-level-1
                         nil
                         :inherit 'default
                         :height 1.0
                         :weight 'bold
                         :foreground "magenta")
     (set-face-attribute 'org-level-2
                         nil
                         :inherit 'default
                         :height 1.0
                         :weight 'bold
                         :foreground "deep sky blue")
     (set-face-attribute 'org-level-3
                         nil
                         :inherit 'default
                         :height 1.0
                         :weight 'bold
                         :foreground "goldenrod3")
     (set-face-attribute 'org-level-4
                         nil
                         :inherit 'default
                         :height 1.0
                         :weight 'bold
                         :foreground "chartreuse3")))
