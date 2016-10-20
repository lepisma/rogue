;;; packages.el --- rogue Layer packages File for Spacemacs

(setq rogue-packages
      '(multiple-cursors
        request-deferred
        svg-clock
        (blackbird :location local)
        cricbuzz
        org-journal
        vue-mode
        solarized-theme))

;; Initialize packages
(defun rogue/init-multiple-cursors ()
  (use-package multiple-cursors
    :defer t))

(defun rogue/init-request-deferred ()
  (use-package request-deferred
    :defer t))

(defun rogue/init-svg-clock ()
  (use-package svg-clock
    :defer t))

(defun rogue/init-blackbird ()
  (use-package blackbird
    :commands (blackbird-read-lyrics)))

(defun rogue/init-cricbuzz ()
  (use-package cricbuzz
    :defer t))

(defun rogue/init-org-journal ()
  (use-package org-journal
    :defer t))

(defun rogue/init-vue-mode ()
  (use-package vue-mode
    :defer t))

(defun rogue/solarized-theme ()
  (use-package solarized-theme
    :defer t))
