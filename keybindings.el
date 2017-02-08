;;; keybindings.el --- rogue Layer keybindings File for Spacemacs

;; Hide Show
(global-set-key (kbd "C-c <down>") 'hs-toggle-hiding)

;; Org Kalarm
(global-set-key (kbd "C-c k") 'org-set-kalarm)

;; To-fish jump
(global-set-key (kbd "C-c j") 'to-fish-jump)

;; Neotree refresh
(global-set-key (kbd "C-c n") 'neotree-refresh)

;; World time
(global-set-key (kbd "C-c t") 'helm-world-time)

;; Weather
(global-set-key (kbd "C-c w") 'weather-amherst)

;; Avy
(global-set-key (kbd "C-'") 'avy-goto-char)

;; Emmet
(global-set-key (kbd "C-c e") 'emmet-expand-line)

;; Don't kill my words
(global-set-key (kbd "C-<backspace>") 'backward-delete-word)

;; Move lines
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
