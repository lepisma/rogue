;;; packages.el --- rogue Layer packages File for Spacemacs

(setq rogue-packages
      '(multiple-cursors
        request-deferred
        svg-clock
        csv-mode))

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

(defun rogue/init-csv-mode ()
  (use-package csv-mode
    :defer t))
