;; Code taken from https://github.com/zachcurry/emacs-anywhere/
;; with fix from https://github.com/zachcurry/emacs-anywhere/issues/18

(require 'f)

(defun ea-on-delete (frame)
  (if (string-equal (buffer-name) "*Emacs Anywhere*")
      (f-write (buffer-string) 'utf-8 "~/ea.clipboard"))
  (ea-unhook)
  (kill-buffer "*Emacs Anywhere*"))

(defun ea-hook ()
  (add-hook 'delete-frame-functions 'ea-on-delete))

(defun ea-unhook ()
  (remove-hook 'delete-frame-functions 'ea-on-delete))

(ea-hook)
(switch-to-buffer "*Emacs Anywhere*")
(select-frame-set-input-focus (selected-frame))
