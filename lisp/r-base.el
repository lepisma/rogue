;;; r-base.el --- Base UI and resets -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;; Base UI and resets
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'package)
(require 'use-package-ensure)

(setq custom-file (locate-user-emacs-file ".emacs-custom.el"))
(unless (file-exists-p custom-file)
  (make-empty-file custom-file))
(load custom-file)

(defconst user-cloud-dir (file-name-as-directory (getenv "CLOUD_DIR")))
(defconst user-notes-dir (file-name-as-directory (concat user-cloud-dir "notes")))

(setq gc-cons-threshold (* 50 1000 1000))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setq use-package-always-ensure t)

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(pixel-scroll-precision-mode t)
(setq-default cursor-type 'bar)
(setq mode-line-right-align-edge 'right-fringe)
(setq-default fill-column 80)
(delete-selection-mode t)
(winner-mode t)

;; Save desktop without any theme settings
(desktop-save-mode t)
(push '(foreground-color . :never) frameset-filter-alist)
(push '(background-color . :never) frameset-filter-alist)
(push '(font . :never) frameset-filter-alist)
(push '(cursor-color . :never) frameset-filter-alist)
(push '(background-mode . :never) frameset-filter-alist)
(push '(ns-appearance . :never) frameset-filter-alist)

(global-unset-key (kbd "M-m"))

(setq make-backup-files nil)
(setq create-lockfiles nil)

(set-face-attribute 'default nil :font "Iosevka" :height 100)
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :height 100)
(set-face-attribute 'variable-pitch nil :font "Merriweather" :height 100)

(setq-default indent-tabs-mode nil)

;; This is in-built in Emacs 30+
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(use-package vertico
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("C-c g" . consult-ripgrep)
         ("C-s" . consult-line)
         ("C-r" . consult-line)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  :hook (completion-list-mode . consult-preview-at-point-mode)
  
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<"))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Need to install fonts by running `nerd-icons-install-fonts'
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  (nerd-icons-color-icons nil))

(use-package doom-modeline
  :init
  (doom-modeline-mode))

(use-package which-key
  :init
  (which-key-mode))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package spacious-padding
  :custom
  (spacious-padding-widths '(:internal-border-width 20
                             :header-line-width 4
                             :mode-line-width 6
                             :right-divider-width 30
                             :scroll-bar-width 8
                             :fringe-width 10))
  (spacious-padding-subtle-mode-line
   `(:mode-line-active 'default
     :mode-line-inactive vertical-border))
  
  :config
  (spacious-padding-mode))

(use-package smart-hungry-delete
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
         ([remap delete-backward-char] . smart-hungry-delete-backward-char)
         ([remap delete-char] . smart-hungry-delete-forward-char)))

(use-package treemacs
  :bind ("M-m p t" . treemacs))

(use-package svg-lib)

(use-package nano-vertico
  :vc (:fetcher github :repo rougier/nano-vertico)
  :config
  (nano-vertico-mode t))

(use-package rainbow-mode)

;; Personal navigation patterns
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(defun delete-line ()
  "Delete current line without killing"
  (interactive)
  (delete-region (line-beginning-position) (line-end-position)))

(defun duplicate-line ()
  "Duplicate a line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank)
  (setq kill-ring (cdr kill-ring)))

;; A few general keybindings
(global-set-key (kbd "C-d") #'duplicate-line)
(global-set-key (kbd "C-S-<backspace>") 'delete-line)
(global-set-key (kbd "C-<backspace>") 'backward-delete-word)
(global-set-key (kbd "M-m a d") 'dired)

(provide 'r-base)

;;; r-base.el ends here
