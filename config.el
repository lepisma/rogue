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

;; Org mode symbols
(setq org-bullets-bullet-list '("■" "●" "○" "▬"))

;; Neotree theme
(setq neo-theme 'nerd)

;; Hide vertical border (improves few themes)
(set-face-attribute 'vertical-border
                    nil
                    :foreground (face-attribute 'default :background))

;; Org idle time
(setq org-clock-idle-time 5)
