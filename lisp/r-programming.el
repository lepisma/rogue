;;; r-programming.el --- Programming related configuration -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;; Programming related configuration
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

(column-number-mode)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'prog-mode-hook (lambda () (setq line-spacing 0.1)))
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  :bind (("M-<right>" . sp-forward-slurp-sexp)
         ("M-<left>" . sp-forward-barf-sexp)
         ("M-S-<right>" . sp-backward-barf-sexp)
         ("M-S-<left>" . sp-backward-slurp-sexp)
         ("M-u" . sp-backward-unwrap-sexp)
         ("M-n" . sp-end-of-sexp)
         ("M-p" . sp-beginning-of-sexp)
         ("M-k" . sp-kill-sexp)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :pin melpa-stable
  :bind ("M-m g s" . magit-status))

(use-package git-timemachine)

(use-package forge
  :after magit)

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package devdocs
  :hook ((python-mode . (lambda () (setq-local devdocs-current-docs '("python~3.12"))))))

(use-package rust-mode)

(use-package direnv
  :config
  (direnv-mode))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M-<mouse-1>" . mc/add-cursor-on-click)))

(use-package crdt
  :custom
  (crdt-use-tuntox 'confirm))

(use-package flycheck
  :hook ((emacs-lisp-mode . flycheck-mode)))

(use-package fish-mode)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((rust-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  (lsp-completion-provider :none)
  :config
  (defun corfu-lsp-setup ()
    (setq-local completion-styles '(orderless)
                completion-category-defaults nil))
  (add-hook 'lsp-mode-hook #'corfu-lsp-setup))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package dap-mode)

(use-package indent-guide
  :hook (prog-mode . indent-guide-mode))

(use-package gptel
  :custom
  (gptel-model "gpt-4o-mini")
  :bind ("M-m a g" . gptel-menu))

;; Emacs Lisp
(define-key emacs-lisp-mode-map (kbd "M-RET e e") #'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "M-RET e b") #'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "M-RET e f") #'eval-defun)
(define-key emacs-lisp-mode-map (kbd "M-RET e r") #'eval-region)

(use-package eros
  :custom
  (eros-eval-result-prefix "▶ ")
  :init
  (eros-mode))

;; Python
(use-package lsp-pyright
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(provide 'r-programming)

;;; r-programming.el ends here
