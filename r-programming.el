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
(electric-pair-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package magit)

(use-package direnv
  :config
  (direnv-mode))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M-<mouse-1>" . mc/add-cursor-on-click)))

;; Emacs Lisp
(define-key emacs-lisp-mode-map (kbd "M-RET e e") #'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "M-RET e b") #'eval-buffer)

(use-package eros
  :custom
  (eros-eval-result-prefix "▶ ")
  :init
  (eros-mode))

(provide 'r-programming)

;;; r-programming.el ends here
