;;; r-ui.el --- Ui setup for rogue layer

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Personal config package for setting up ui
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
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'r-utils)
(require 'dash-functional)

(defun r-ui/clear-sides ()
  "Setup gaps on left and right sides."
  (setq left-margin-width 2
        right-margin-width 2)
  (set-window-buffer nil (current-buffer)))

(defun r-ui/clear-header-sides ()
  "Add left gap in header line"
  (setq header-line-format '(:eval (concat "  " (get-text-property (point-min) (quote header-line))))))

(defun r-ui/clear-header ()
  "Clear header line."
  (setq header-line-format " "))

(defun r-ui/line-spacing (size)
  "Set line spacing."
  (setq line-spacing size))

(defun r-ui/hide-mode-line ()
  "Hide mode line. Wrap around the spacemacs' function."
  (hidden-mode-line-mode +1))

(defun r-ui/no-hl-line ()
  "Disable line highlight. Wrap around spacemacs' function."
  (spacemacs/disable-hl-line-mode))

(defun r-ui/setup-fringe ()
  "Setup git fringe"
  (setq-default fringes-outside-margins t
                indicate-buffer-boundaries nil
                fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                                             fringe-indicator-alist))
  (setq flycheck-indication-mode nil)

  (add-hook 'magit-post-refresh-hook #'git-gutter:update-all-windows)
  (define-fringe-bitmap 'git-gutter-fr:added
    [224 224 224 224 224 224 224 224 224 224 224 224 224
         224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224 224 224 224 224 224 224 224 224 224 224 224 224
         224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil nil 'center)
  (fringe-mode 3))

(defun r-ui/setup-ibuffer ()
  "Ibuffer cleanup"

  (setq ibuffer-expert t
        ibuffer-show-empty-filter-groups nil
        ibuffer-filter-group-name-face 'org-tag
        ibuffer-modified-char ?\★
        ibuffer-locked-char ?\-
        ibuffer-read-only-char ?\-
        ibuffer-marked-char ?\✓
        ibuffer-deletion-char ?\✕
        ibuffer-deletion-face 'org-agenda-done
        ibuffer-use-header-line nil
        ibuffer-projectile-prefix "")

  (defun r-ui//ibuffer-remove-title (&rest args)
    (save-excursion
      (set-buffer "*Ibuffer*")
      (toggle-read-only 0)
      (goto-char 1)
      (search-forward "-\n" nil t)
      (delete-region 1 (point))
      (insert "\n")
      (let ((window-min-height 1))
        (shrink-window-if-larger-than-buffer))
      (toggle-read-only)))

  (advice-add 'ibuffer-update-title-and-summary :after 'r-ui//ibuffer-remove-title))

(defun r-ui/setup-misc ()
  "Setup ui for misc packages/tools."

  (use-package em-tramp
    :config
    (setq eshell-prefer-lisp-functions t
          password-cache t
          password-cache-expiry 3600))

  (setq eshell-prompt-function
        (lambda ()
          (concat (propertize
                   (abbreviate-file-name (eshell/pwd)) 'face '(:foreground "#727280"))
                  " "
                  (propertize
                   (or conda-env-current-name "") 'face '(:foreground "#00bfff"))
                  "\nλ ")))

  (setq tramp-default-method "ssh"
        eshell-scroll-show-maximum-output nil
        comint-scroll-show-maximum-output nil
        comint-input-ignoredups t)

  (setq ielm-prompt "λ "
        ielm-prompt-internal "λ ")

  ;; Ranger stuff
  (setq ranger-cleanup-on-disable t
        ranger-show-hidden t
        ranger-parent-depth 2
        ranger-width-preview 0.60
        ranger-width-parents 0.15
        ranger-modify-header t
        ranger-max-preview-size 5
        ranger-dont-show-binary t
        wdired-allow-to-change-permissions t)

  (ranger-override-dired-mode t)

  (setq ranger-header-func (lambda () ""))

  (blink-cursor-mode t)
  (setq-default cursor-in-non-selected-windows nil)
  (setq frame-title-format '("" "%b - Emacs"))

  (setq neo-banner-message nil
        neo-mode-line-type 'none
        neo-autorefresh nil)

  (setq alert-default-style 'libnotify))

(defun r-ui/setup-minibuffer ()
  "Fix minibuffer and surrounding area."
  (advice-add 'helm-display-mode-line
              :override (lambda (source &optional force) (r-ui/hide-mode-line))))

(defun r-ui/setup ()
  "Setup everything."

  (with-eval-after-load 'git-gutter
    (r-ui/setup-fringe))
  (r-ui/setup-ibuffer)
  (r-ui/setup-misc)
  (r-ui/setup-minibuffer)

  (r-utils/add-hooks '(Info-mode-hook
                       anaconda-view-mode
                       comint-mode-hook
                       elfeed-search-update-hook
                       eshell-mode-hook
                       help-mode-hook
                       helpful-mode-hook
                       ibuffer-mode-hook
                       magit-diff-mode-hook
                       magit-log-mode-hook
                       magit-popup-mode-hook
                       magit-status-mode-hook
                       mu4e-main-mode-hook
                       mu4e-view-mode-hook
                       nov-mode-hook
                       org-agenda-mode-hook
                       process-menu-mode-hook
                       slime-repl-mode-hook
                       text-mode-hook)
                     (list #'r-ui/clear-sides #'r-ui/clear-header))

  ;; Hooks for side gap in header
  (r-utils/add-hooks '(Info-mode-hook)
                     (list #'r-ui/clear-header-sides))

  ;; Hooks for hidden modeline
  (r-utils/add-hooks '(comint-mode-hook
                       compilation-mode-hook
                       completion-list-mode-hook
                       eshell-mode-hook
                       help-mode-hook
                       helpful-mode-hook
                       ibuffer-mode-hook
                       magit-diff-mode-hook
                       magit-log-mode-hook
                       magit-popup-mode-hook
                       magit-status-mode-hook
                       messages-buffer-mode-hook
                       org-agenda-mode-hook
                       process-menu-mode-hook
                       ranger-mode-hook
                       ranger-parent-dir-hook
                       ranger-preview-dir-hook
                       slime-repl-mode-hook
                       processing-compilation-mode-hook)
                     (list #'r-ui/hide-mode-line))

  ;; Hooks for line spacing
  (r-utils/add-hooks '(comint-mode-hook
                       helm-mode-hook
                       ibuffer-mode-hook
                       prog-mode-hook
                       ranger-mode-hook
                       text-mode-hook)
                     (list (lambda () (r-ui/line-spacing 0.1))))

  (r-utils/add-hooks '(org-agenda-mode-hook)
                     (list (lambda () (r-ui/line-spacing 0.2))))

  ;; No line highlighting
  (r-utils/add-hooks '(text-mode-hook)
                     (list #'r-ui/no-hl-line))

  ;; Other general hooks
  (add-hook 'css-mode-hook #'rainbow-mode)
  (add-hook 'text-mode-hook #'auto-fill-mode)
  (add-hook 'term-mode-hook #'toggle-truncate-lines)

  ;; Clear message buffer
  (with-current-buffer "*Messages*"
    (r-ui/hide-mode-line)
    (r-ui/clear-header)
    (r-ui/clear-sides)))


(provide 'r-ui)

;;; r-ui.el ends here
