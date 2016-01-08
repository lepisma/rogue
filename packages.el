;;; packages.el --- rogue Layer packages File for Spacemacs

(setq rogue-packages
      '(
        multiple-cursors
        restclient
        request-deferred
        ))

;; List of packages to exclude.
(setq rogue-excluded-packages '())

;; Initialize packages
(defun rogue/init-multiple-cursors ()
  (require 'multiple-cursors)
  )

(defun rogue/init-restclient ()
  (require 'restclient)
  )

(defun rogue/init-request-deferred ()
  (require 'request-deferred)
)
