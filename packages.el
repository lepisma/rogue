;;; packages.el --- rogue Layer packages File for Spacemacs

(setq rogue-packages
      '(
        multiple-cursors
        ))

;; List of packages to exclude.
(setq rogue-excluded-packages '())

;; Initialize packages
(defun rogue/init-multiple-cursors ()
  (require 'multiple-cursors)
  )
