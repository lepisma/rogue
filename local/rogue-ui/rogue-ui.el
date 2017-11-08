;;; rogue-ui.el --- Ui setup for rogue layer

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/lepisma/rogue/tree/master/local/rogue-ui

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

(require 'rogue-utils)
(require 'dash-functional)

(defun rogue-ui-clear-sides ()
  "Setup gaps on left and right sides."
  (setq left-margin-width 2
        right-margin-width 2)
  (set-window-buffer nil (current-buffer)))

(defun rogue-ui-clear-header ()
  "Clear header line."
  (setq header-line-format " "))

(defun rogue-ui-line-spacing (size)
  "Set line spacing."
  (setq line-spacing size))

(defun rogue-ui-hide-mode-line ()
  "Hide mode line. Wrap around the spacemacs' function."
  (hidden-mode-line-mode +1))

(defun rogue-ui-no-hl-line ()
  "Disable line highlight. Wrap around spacemacs' function."
  (spacemacs/disable-hl-line-mode))

(defun rogue-ui-setup-fringe ()
  "Setup git fringe"
  (setq-default fringes-outside-margins t
                indicate-buffer-boundaries nil
                fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                                             fringe-indicator-alist))
  (setq flycheck-indication-mode nil)

  (add-hook 'magit-post-refresh-hook 'git-gutter:update-all-windows)
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

(defun rogue-ui-setup-ibuffer ()
  "Ibuffer cleanup"

  (setq ibuffer-expert t
        ibuffer-show-empty-filter-groups nil
        ibuffer-filter-group-name-face 'org-level-1
        ibuffer-modified-char ?\★
        ibuffer-locked-char ?\-
        ibuffer-read-only-char ?\-
        ibuffer-marked-char ?\✓
        ibuffer-deletion-char ?\✕
        ibuffer-deletion-face 'org-agenda-done
        ibuffer-use-header-line nil)

  (defun rogue-ui--ibuffer-remove-title (&rest args)
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

  (advice-add 'ibuffer-update-title-and-summary :after 'rogue-ui--ibuffer-remove-title))

(defun rogue-ui-setup-misc ()
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

  ;; nlinum
  (setq nlinum-highlight-current-line t
        nlinum-format " %d ")

  (blink-cursor-mode t)
  (setq-default cursor-in-non-selected-windows nil)
  (setq frame-title-format '("" "%b - Emacs"))

  (setq neo-banner-message nil
        neo-mode-line-type 'none
        neo-autorefresh nil)

  (setq alert-default-style 'libnotify))

(defun rogue-ui-setup-minibuffer ()
  "Fix minibuffer and surrounding area."
  (advice-add 'helm-display-mode-line
              :override (lambda (source &optional force) (rogue-ui-hide-mode-line))))

(defun rogue-ui-setup ()
  "Setup everything."

  (rogue-ui-setup-fringe)
  (rogue-ui-setup-ibuffer)
  (rogue-ui-setup-misc)
  (rogue-ui-setup-minibuffer)

  ;; Hooks for side gaps
  (rogue-utils-add-hooks '(cfw:calendar-mode-hook
                           text-mode-hook
                           org-agenda-mode-hook
                           ibuffer-mode-hook
                           magit-status-mode-hook
                           magit-popup-mode-hook
                           magit-log-mode-hook
                           magit-diff-mode-hook
                           comint-mode-hook
                           eshell-mode-hook
                           slime-repl-mode-hook
                           process-menu-mode-hook
                           mu4e-view-mode-hook
                           mu4e-main-mode-hook
                           nov-mode-hook)
                         'rogue-ui-clear-sides)

  ;; Hooks for header gaps
  (rogue-utils-add-hooks '(cfw:calendar-mode-hook
                           text-mode-hook
                           org-agenda-mode-hook
                           ibuffer-mode-hook
                           magit-status-mode-hook
                           magit-log-mode-hook
                           magit-diff-mode-hook
                           comint-mode-hook
                           eshell-mode-hook
                           slime-repl-mode-hook
                           process-menu-mode-hook
                           mu4e-view-mode-hook
                           mu4e-main-mode-hook
                           nov-mode-hook)
                         'rogue-ui-clear-header)

  ;; Hooks for hidden modeline
  (rogue-utils-add-hooks '(processing-compilation-mode-hook
                           eshell-mode-hook
                           help-mode-hook
                           compilation-mode-hook
                           messages-buffer-mode-hook
                           completion-list-mode-hook
                           org-agenda-mode-hook
                           ranger-mode-hook
                           ibuffer-mode-hook
                           magit-status-mode-hook
                           magit-popup-mode-hook
                           magit-log-mode-hook
                           magit-diff-mode-hook
                           comint-mode-hook
                           ranger-parent-dir-hook
                           ranger-preview-dir-hook
                           slime-repl-mode-hook
                           process-menu-mode-hook)
                         'rogue-ui-hide-mode-line)

  ;; Hooks for line spacing
  (rogue-utils-add-hooks '(text-mode-hook
                           prog-mode-hook
                           ranger-mode-hook
                           ibuffer-mode-hook
                           comint-mode-hook)
                         (-partial 'rogue-ui-line-spacing 0.1))

  (rogue-utils-add-hooks '(org-agenda-mode-hook)
                         (-partial 'rogue-ui-line-spacing 0.2))

  ;; No line highlighting
  (rogue-utils-add-hooks '(text-mode-hook
                           cfw:calendar-mode-hook)
                         'rogue-ui-no-hl-line)

  ;; Clear message buffer
  (with-current-buffer "*Messages*"
    (rogue-ui-hide-mode-line)
    (rogue-ui-clear-header)
    (rogue-ui-clear-sides))

  ;; Other general hooks
  (add-hook 'css-mode-hook (lambda () (rainbow-mode 1)))
  (add-hook 'text-mode-hook 'auto-fill-mode)
  (add-hook 'term-mode-hook 'toggle-truncate-lines)
  (add-hook 'prog-mode-hook 'nlinum-mode))

(provide 'rogue-ui)

;;; rogue-ui.el ends here
