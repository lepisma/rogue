;;; rogue --- clutter free emacs config ;

;;; Code:

;; *important* Change the path to cask install directory
(require 'cask "~/.cask/cask.el")

;; Setup cask and pallet
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; UI section
;; ----------
;; Graphene does the basic setup
(require 'graphene)
;; Tweaks
(load-theme 'tsdh-dark)
;; Disable menu bar
(menu-bar-mode -1)
;; CUA Mode
(cua-mode 1)
;; Highlight current linex
(global-hl-line-mode 1)
;; Powerline for better mode line
(powerline-default-theme)
;; Remove border from mode line and change font family
(set-face-attribute 'mode-line nil
                    :box nil
                    :family "consolas")
;; Set cursor to bar
(setq-default cursor-type 'bar)

;; Rainbow delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Desktop save
(desktop-save-mode 1)

;; Tabbar
;; (setq tabbar-ruler-global-tabbar t)
;; (setq tabbar-ruler-global-ruler t)
;; (require 'tabbar-ruler) 

;; (require 'tabbar)
;; (tabbar-mode t)
;; (defun tabbar-buffer-groups ()
;;   (list
;;    (cond
;;     ((string-equal "*" (substring (buffer-name) 0 1))
;;      "Emacs Buffer"
;;      )
;;     ((eq major-mode 'dired-mode)
;;      "Dired"
;;      )
;;     (t
;;      "User Buffer"
;;      )
;;     )))

;; (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

;; (global-set-key [M-left] 'tabbar-backward)
;; (global-set-key [M-right] 'tabbar-forward)

;;(setq tabbar-use-images nil)

;; Sr Speedbar stuff
(require 'sr-speedbar)
  (setq 
   sr-speedbar-right-side nil
   sr-speedbar-width-x 40
   sr-speedbar-width-console 40
   sr-speedbar-max-width 40
   sr-speedbar-delete-windows t)

;; Multiple cursors
(require 'multiple-cursors)

(global-set-key (kbd "C-c C-q") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Packages config section
;; -----------------------
(projectile-global-mode)
(yas-global-mode 1)

;; Hex colors in CSS
(add-hook 'css-mode-hook (lambda ()
                           (rainbow-mode 1)
                           ))
;; ELPY for python
(elpy-enable)
(elpy-use-ipython)
;; Clojure mode hook
(add-hook 'clojure-mode-hook (lambda ()
                               (cider-mode 1)
                               ))

;; org mode special-symbol-mode
(setq-default org-pretty-entities 1)

;; trigger flypell in latex mode
(add-hook 'latex-mode-hook (lambda ()
                             (flyspell-mode 1)
                             ))

;; Using aspell instead of ispell
(setq ispell-list-command "--list")

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bfa3d52c7e3bbf528760bdbb8b59a69beda8d8b257d60a1b3ac26c1e5bc190bb" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
