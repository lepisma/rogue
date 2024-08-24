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

(use-package all-the-icons)

(defvar r-themes/dark-mode t
  "Whether currently the editor is in dark-mode.")

(defun r-themes/set-dark-theme ()
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'doom-city-lights t)
  (setq r-themes/dark-mode t))

(defun r-themes/set-light-theme ()
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'doom-rogue-light t)
  (setq r-themes/dark-mode nil))

(defun r-themes/cycle-theme ()
  "Cycle between dark and light theme variant."
  (interactive)
  (if r-themes/dark-mode
      (r-themes/set-light-theme)
    (r-themes/set-dark-theme)))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-atom")

  :config
  (doom-themes-treemacs-config)
  (doom-themes-org-config)

  :bind ("M-m t t" . r-themes/cycle-theme)

  :hook (after-init . r-themes/set-light-theme))

(provide 'r-themes)

;;; r-themes.el ends here
