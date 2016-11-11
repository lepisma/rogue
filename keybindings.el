;;; keybindings.el --- rogue Layer keybindings File for Spacemacs

;; Multiple cursor keys
(global-set-key (kbd "C-c C-q") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

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

;; Hackernews
(global-set-key (kbd "C-c h") 'hackernews)

;; Swiper
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)

;; Avy
(global-set-key (kbd "C-'") 'avy-goto-char)
