;;; funcs.el --- rogue Layer utility functions

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer) (buffer-list))))

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

(defun org-insert-org ()
  "Create another org buffer and 'include' it in current. Ask for filename."
  (interactive)

  (let ((filename (concat (read-string "Enter filename (without `.org'): ") ".org")))
    (insert (concat "#+INCLUDE: \"./" filename "\""))
    (find-file filename) ; Open buffer with filename
    (if (file-exists-p filename)
        (message "File already exists"))))

(defun org-get-scheduled-or-deadline ()
  "Return scheduled or deadline time from current point in order of priority"

  (let ((time (org-get-scheduled-time (point))))
    (if (not time)
        (setq time (org-get-deadline-time (point))))
    (if time
        (format-time-string "%Y-%m-%d-%H:%M" time)
      (nill))))

(defun org-set-kalarm ()
  "Set an alarm for current org entry (schedule/deadline) in kalarm"
  (interactive)

  (let ((time (org-get-scheduled-or-deadline))
        (message (substring-no-properties (org-get-heading t t)))
        (audio-file (expand-file-name "~/.emacs.d/private/rogue/data/alarm.ogg")))
    (if (and time message)
        (if (eq 0 (call-process "kalarm"
                                nil nil nil
                                "-t" time
                                "-p" audio-file
                                message))
            (message (concat "Alarm set for : " message))
          (message "Error in setting alarm"))
      (message "Error in parsing entry"))))

(defun to-fish-find-file (candidate)
  "Run find file for given bookmark"
  (ido-find-file-in-dir (concat (file-name-as-directory "~/.tofish") candidate)))

(defun to-fish-jump ()
  "Jump to to-fish bookmarks"
  (interactive)
  (helm :sources (helm-build-sync-source "bookmarks"
                   :candidates (lambda ()
                                 (directory-files "~/.tofish"))
                   :action '(("Jump to bookmark" . to-fish-find-file)))
        :buffer "*helm tofish jump*"
        :prompt "Jump to : "))
