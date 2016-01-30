;;; funcs.el --- rogue Layer utility functions


(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun explore-here ()
  "Open file manager in current buffer's directory."
  (interactive)
  (if (eq system-type 'windows-nt)
      (shell-command "explorer .")
    (message "Only on Windows")))
