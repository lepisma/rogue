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

(require 'company)
(require 'r-utils)
(require 'dash-functional)
(require 'treemacs)
(require 'all-the-icons)
(require 'doom-modeline)
(require 'pyvenv)

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

(defun r-ui/setup-treemacs ()
  "Treemacs stuff. Most of these are modified version of code
from doom-themes."
  (setq treemacs-icon-root-png
        (concat " " (all-the-icons-octicon "repo" :v-adjust -0.1 :height 1.2
                                           :face '(:inherit font-lock-comment-face :foreground "#666666"))
                " ")

        treemacs-indentation-string "  "
        treemacs-indentation 1

        treemacs-icon-open-png
        (concat (all-the-icons-octicon "file-directory" :face '(:inherit font-lock-comment-face))
                " ")
        treemacs-icon-closed-png
        (concat (all-the-icons-octicon "file-directory" :face '(:inherit font-lock-doc-face :slant normal))
                " ")

        treemacs-icon-tag-node-open-png
        (concat (all-the-icons-octicon "chevron-down"  :height 0.75 :v-adjust 0.1 :face 'font-lock-keyword-face)
                "\t")

        treemacs-icon-tag-node-closed-png
        (concat (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face 'font-lock-keyword-face)
                "\t")

        treemacs-icon-tag-leaf-png "· ")

  ;; https://github.com/hlissner/emacs-doom-themes/pull/285
  (dolist (face '(treemacs-tags-face))
    (let ((faces (face-attribute face :inherit nil)))
      (set-face-attribute
       face nil :inherit
       `(variable-pitch ,@(delq 'unspecified (if (listp faces) faces (list faces))))))))

(defun r-ui/setup-misc ()
  "Setup ui for misc packages/tools."

  (setq doom-modeline-height 20
        doom-modeline-bar-width 4
        doom-modeline-icon t
        doom-modeline-buffer-encoding nil
        doom-modeline-modal-icon t
        doom-modeline-mu4e t
        doom-modeline-buffer-modification-icon nil
        doom-modeline-buffer-state-icon nil
        doom-modeline-major-mode-color-icon nil)

  (add-to-list 'global-mode-string
               '(:eval (when pyvenv-virtual-env
                         (car (last (f-split pyvenv-virtual-env))))))

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

;;;###autoload
(defun r-ui/setup ()
  "Setup everything."

  (with-eval-after-load 'git-gutter
    (r-ui/setup-fringe))
  (r-ui/setup-ibuffer)
  (r-ui/setup-misc)
  (r-ui/setup-minibuffer)
  (r-ui/setup-treemacs)

  (r-utils/add-hooks '(Info-mode-hook
                       chronos-mode-hook
                       cider-repl-mode-hook
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
                       text-mode-hook
                       treemacs-mode-hook)
                     (list #'r-ui/clear-sides #'r-ui/clear-header))

  ;; Hooks for side gap in header
  (r-utils/add-hooks '(Info-mode-hook)
                     (list #'r-ui/clear-header-sides))

  ;; Hooks for hidden modeline
  (r-utils/add-hooks '(chronos-mode-hook
                       comint-mode-hook
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
                       mu4e-compose-mode-hook
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

  (r-utils/add-hooks '(text-mode-hook) (list #'variable-pitch-mode
                                             (lambda () (setq-local company-frontends '(company-preview-frontend)))))

  (r-utils/add-hooks '(yaml-mode-hook toml-mode-hook) (list (lambda () (variable-pitch-mode 0))))
  (add-hook 'term-mode-hook #'toggle-truncate-lines)

  ;; Clear message buffer
  (with-current-buffer "*Messages*"
    (r-ui/hide-mode-line)
    (r-ui/clear-header)
    (r-ui/clear-sides)))

(provide 'r-ui)

;;; r-ui.el ends here
