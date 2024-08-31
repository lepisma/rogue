;;; r-themes.el --- Themes -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;; Themes
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

(use-package all-the-icons
  :custom
  (all-the-icons-color-icons nil))

(defvar r-themes/dark-mode t
  "Whether currently the editor is in dark-mode.")

(defcustom r-themes/dark-theme 'doom-rogue-dark
  "Theme for dark mode.")

(defcustom r-themes/light-theme 'doom-rogue-light
  "Theme for light mode.")

(defun r-themes/set-dark-theme ()
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme r-themes/dark-theme t)
  (setq r-themes/dark-mode t))

(defun r-themes/set-light-theme ()
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme r-themes/light-theme t)
  (setq r-themes/dark-mode nil))

(defun r-themes/cycle-theme ()
  "Cycle between dark and light theme variant."
  (interactive)
  (if r-themes/dark-mode
      (r-themes/set-light-theme)
    (r-themes/set-dark-theme)))

(use-package auto-dark
  :custom
  (auto-dark-dark-theme r-themes/dark-theme)
  (auto-dark-light-theme r-themes/light-theme)
  :config
  (auto-dark-mode t))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-atom")

  :config
  (doom-themes-treemacs-config)
  (doom-themes-org-config)

  :bind ("M-m t t" . r-themes/cycle-theme))

(if (auto-dark--is-dark-mode)
    (r-themes/set-dark-theme)
  (r-themes/set-light-theme))

(provide 'r-themes)

;;; r-themes.el ends here
