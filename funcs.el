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
    (if (eq system-type 'gnu/linux)
        (shell-command "xdg-open .")
      (message "System Not supported"))))

(defun org-screenshot ()
  "Insert image from the clipboard into the org buffer."
  (interactive)
  ; Set image directory
  (setq imagedir (concat (buffer-file-name) "_images/"))
  (if (not (file-exists-p imagedir))
      (mkdir imagedir))
  (setq filename (concat "img_" (format-time-string "%Y_%m_%d__%H_%M_%S") ".png"))
  (setq filepath (concat imagedir filename))

  ; Write screenshot
  (call-process "convert" nil nil nil "clipboard:image" filepath)

  (if (file-exists-p filepath)
      (insert (concat "[[" (concat "./" (buffer-name) "_images/" filename ) "]]")))
  (org-display-inline-images))
