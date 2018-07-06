;;; keybindings.el --- rogue Layer keybindings File for Spacemacs

;; Hide Show
(global-set-key (kbd "C-c <down>") 'hs-toggle-hiding)

;; To-fish jump
(global-set-key (kbd "C-c j") 'to-fish-jump)

;; Neotree refresh
(global-set-key (kbd "C-c n") 'neotree-refresh)

;; Speak unread emails
(global-set-key (kbd "C-c m") 'quack-unread-mail)

;; Avy
(global-set-key (kbd "C-'") 'avy-goto-char)

;; Insect
(global-set-key (kbd "C-c i") 'insect-calc)

;; Don't kill my words
(global-set-key (kbd "C-<backspace>") 'backward-delete-word)

;; Line stuff
(global-set-key (kbd "C-d") 'duplicate-line)
(global-set-key (kbd "M-<up>") 'move-text-line-up)
(global-set-key (kbd "M-<down>") 'move-text-line-down)
(global-set-key (kbd "C-S-<backspace>") 'delete-line)

;; Projectile
(global-set-key (kbd "C-c g") 'helm-projectile-ag)

;; Zooming
(global-set-key (kbd "C-c z") 'spacemacs/zoom-frm-transient-state/body)

;; Cycle theme
(global-set-key (kbd "M-m T n") 'rogue-cycle-theme)

;; Mail stuff
(define-key mu4e-compose-mode-map (kbd "C-c C-c") 'rogue-mu4e-sign-and-send)
(define-key mu4e-compose-mode-map (kbd "C-c e") 'mml-secure-encrypt-pgp)
(define-key mu4e-view-mode-map (kbd "C-c d") 'epa-mail-decrypt)

;; Some resets
(global-set-key (kbd "C-x C-l") nil)
(global-set-key (kbd "<insert>") nil)
(define-key org-mode-map (kbd "C-c C-x C-s") nil)

;; Smartparens
(global-set-key (kbd "M-<right>") 'sp-forward-slurp-sexp)
(global-set-key (kbd "M-<left>") 'sp-forward-barf-sexp)
(global-set-key (kbd "M-S-<right>") 'sp-backward-barf-sexp)
(global-set-key (kbd "M-S-<left>") 'sp-backward-slurp-sexp)
(global-set-key (kbd "M-u") 'sp-backward-unwrap-sexp)
(global-set-key (kbd "M-n") 'sp-end-of-sexp)
(global-set-key (kbd "M-p") 'sp-beginning-of-sexp)

;; Persp grouping
(global-set-key (kbd "C-x C-b") #'(lambda (arg) (interactive "P") (with-persp-buffer-list () (ibuffer arg))))

;; Devanagari
(global-set-key (kbd "C-c k") #'toggle-devanagari)
