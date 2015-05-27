;;; rogue --- clutter free emacs config ;

;;; Code:

;; *important* Change the path to cask install directory

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
(load-theme 'gotham t)
;; Disable menu bar
(menu-bar-mode -1)
;; CUA Mode
(cua-mode 1)
;; Linum mode
(global-linum-mode 1)
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
(setq ring-bell-function 'ignore)

;; Desktop save
(desktop-save-mode 1)

;; Proper Unicode
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Sr Speedbar stuff
(require 'sr-speedbar)
(setq speedbar-frame-parameters
      '((minibuffer)
        (width . 40)
        (border-width . 0)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (unsplittable . t)
        (left-fringe . 0)))
(setq speedbar-hide-button-brackets-flag t)
(setq speedbar-show-unknown-files t)
(setq speedbar-smart-directory-expand-flag t)
(setq speedbar-use-images nil)
(setq sr-speedbar-auto-refresh t)
(setq sr-speedbar-max-width 70)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-width-console 40)
 
(when window-system
  (defadvice sr-speedbar-open (after sr-speedbar-open-resize-frame activate)
    (set-frame-width (selected-frame)
                     (+ (frame-width) sr-speedbar-width)))
  (ad-enable-advice 'sr-speedbar-open 'after 'sr-speedbar-open-resize-frame)
 
  (defadvice sr-speedbar-close (after sr-speedbar-close-resize-frame activate)
    (sr-speedbar-recalculate-width)
    (set-frame-width (selected-frame)
                     (- (frame-width) sr-speedbar-width)))
  (ad-enable-advice 'sr-speedbar-close 'after 'sr-speedbar-close-resize-frame))


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

;; trigger flypell in latex and org mode
(add-hook 'latex-mode-hook (lambda ()
                             (flyspell-mode 1)
                             ))
(add-hook 'org-mode-hook (lambda ()
                           (flyspell-mode 1)
                           ))

;; Rainbow delimiters
(add-hook 'prog-mode-hook (lambda ()
                            (rainbow-delimiters-mode 1)
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
    ("2d7e4feac4eeef3f0610bf6b155f613f372b056a2caae30a361947eab5074716" "c158c2a9f1c5fcf27598d313eec9f9dceadf131ccd10abc6448004b14984767c" "7f5837a7dbf54c2b7c41d94f5eb1373cf63274847d1971037faa24d7f2231eea" "86201c0dccf07a21ce323e124ee9c89d04bbe4f5067446e6492b6ea82265b2d6" "85ef1f4095ad38ed2744577c00e5f1f8dc0000d5015024d50943a1808495f56c" "4695c919c56c1d81fb62d1a7c1cc40d0b365d766f053be2a25df08565bdbd793" "bfa3d52c7e3bbf528760bdbb8b59a69beda8d8b257d60a1b3ac26c1e5bc190bb" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ))
