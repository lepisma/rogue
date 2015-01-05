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

;; Rainbow delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Desktop save
(desktop-save-mode 1)

;; Sr Speedbar stuff
(require 'sr-speedbar)
  (setq 
   sr-speedbar-right-side nil
   sr-speedbar-width-x 40
   sr-speedbar-width-console 40
   sr-speedbar-max-width 40
   sr-speedbar-delete-windows t)

;; Packages config section
;; -----------------------
(projectile-global-mode)
(yas-global-mode 1)
;; ELPY for python
(elpy-enable)
(elpy-use-ipython)
;; Clojure mode hook
(add-hook 'clojure-mode-hook (lambda ()
                               (cider-mode 1)
                               ))
;;; init.el ends here
