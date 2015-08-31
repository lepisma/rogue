;;; config.el --- rogue Layer config File for Spacemacs

;; Custom spacemacs layer
;; - Abhinav Tushar


;; Re enable CUA
(cua-mode 1)

;; Add line numbers
(global-linum-mode 1)

;; Switch to bar
(setq-default cursor-type 'bar)

;; Save desktop
(desktop-save-mode 1)

;; Add rainbow mode to css and scss
(add-hook 'css-mode-hook (lambda ()
                           (rainbow-mode 1)
                           ))

;; Display time in modeline
(display-time-mode 1)
