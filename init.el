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
(load-theme 'ample t)
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

;; Neotree stuff
(require 'neotree)
(setq neo-window-width 28)

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

;; Irony mode
(setq w32-pipe-read-delay 0)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun custom-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'custom-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


;; Using aspell instead of ispell
(setq ispell-list-command "--list")

;; Twitter
(setq twittering-icon-mode t)

;;; init.el ends here
