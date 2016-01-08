;;; config.el --- rogue Layer config File for Spacemacs

;; Re enable CUA
(cua-mode 1)

;; Add line numbers
(global-linum-mode t)

;; Switch to bar
(setq-default cursor-type 'bar)

;; Save desktop
(desktop-save-mode 1)

;; Add rainbow mode to css and scss
(add-hook 'css-mode-hook (lambda ()
                           (rainbow-mode 1)
                           ))

;; Spell check
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Set notes directory for deft
(setq deft-directory (getenv "NOTES_DIR"))
(setq deft-recursive t)

;; Display time in modeline
(display-time-mode 1)

;; Org mode symbols
(setq org-bullets-bullet-list '("■" "●" "○" "▬"))

;; Neotree theme
(setq neo-theme 'nerd)
