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
      (display-warning :error "System Not supported"))))

(defun org-insert-org ()
  "Create another org buffer and 'include' it in current. Ask for filename."
  (interactive)
  (let ((filename (concat (read-string "Enter filename (without `.org'): ") ".org")))
    (insert (concat "#+INCLUDE: \"./" filename "\""))
    (find-file filename) ; Open buffer with filename
    (if (file-exists-p filename)
        (display-warning :warning "File already exists"))))

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
        (audio-file (expand-file-name (concat user-layer-path "/data/alarm.ogg"))))
    (if (and time message)
        (if (eq 0 (call-process "kalarm"
                                nil nil nil
                                "-t" time
                                "-p" audio-file
                                message))
            (message (concat "Alarm set for : " message))
          (display-warning :error "Error in setting alarm"))
      (display-warning :error "Error in parsing entry"))))

(defun to-fish-find-file (candidate)
  "Run find file for given bookmark"
  (helm-find-files-1 (concat
                      (file-name-as-directory (expand-file-name "~/.tofish"))
                      candidate
                      "/")))

(defun to-fish-jump ()
  "Jump to to-fish bookmarks"
  (interactive)
  (helm :sources (helm-build-sync-source "bookmarks"
                   :candidates (lambda ()
                                 (directory-files "~/.tofish"))
                   :action '(("Jump to bookmark" . to-fish-find-file)))
        :buffer "*helm tofish jump*"
        :prompt "Jump to : "))

(defun git-archive ()
  "Archive current repository"
  (interactive)
  (let ((repo-root (magit-toplevel)))
    (if repo-root
        (let ((output-file (read-file-name "Output file: ")))
          (if (eq 0 (call-process "git"
                                  nil nil nil
                                  "archive"
                                  "-o" output-file
                                  "HEAD"))
              (message "git-archive finished")
            (display-warning :error "Error in archiving")))
      (display-warning :error "Not in a git repository"))))

(defun fahrenheit-to-celcius (f)
  "Convert F to C"
  (/ (- f 32) 1.8))

(defun transform-pair-units (pairs)
  "Transform unit pairs to SI. Just temp for now."
  (mapcar (lambda (pair)
            (let ((split (split-string (second pair) "°")))
              (if (string-equal (second split) "F")
                  (progn
                    (list
                     (first pair)
                     (concat (format "%0.2f"
                              (fahrenheit-to-celcius
                               (string-to-number (first split)))) "°C")))
                pair))) pairs))

(defun show-weather-in-buffer (pairs location)
  "Display weather data in a new buffer"
  (let ((buffer (get-buffer-create "*Weather*")))
    (set-buffer buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (org-mode)
    (insert "#+TITLE: ")
    (insert location)
    (insert "\n\n")
    (mapc (lambda (pair)
            (insert (concat "+ " (first pair) " :: " (second pair) "\n")))
          (transform-pair-units (butlast pairs)))
    (switch-to-buffer buffer)
    (setq buffer-read-only t)
    (goto-char (point-min))))

(defun mpc-send-message (channel message)
  "Send message to mpc"
  (if (eq 0 (call-process "mpc"
                          nil nil nil
                          "sendmessage"
                          channel message))
      (message "Done")
    (display-warning :error "Error in sending message to mpc")))

(defun mpdas-love ()
  "Love song on scrobbler service"
  (interactive)
  (mpc-send-message "mpdas" "love"))

(defun mpdas-unlove ()
  "Unlove currently playing song"
  (interactive)
  (mpc-send-message "mpdas" "unlove"))

(defun weather-amherst ()
  "Get local weather information for Amherst from CS station"
  (interactive)
  (let* ((rss-url "http://weather.cs.umass.edu/RSS/weewx_rss.xml")
         (location "Amherst, MA (USA)")
         (node (first (enlive-get-elements-by-tag-name
                       (enlive-fetch rss-url) 'encoded)))
         (items (split-string (enlive-text node) "\n" t)))
    (show-weather-in-buffer
     (mapcar (lambda (item)
               (mapcar 'string-trim (split-string item ": "))) items) location)
    (weather-amherst-mode)))

(defvar weather-amherst-mode-map (make-sparse-keymap))
(define-key weather-amherst-mode-map (kbd "q") 'kill-this-buffer)

(define-minor-mode weather-amherst-mode
  "Minor mode for adding keybindings"
  nil nil
  weather-amherst-mode-map)

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(defun insect-calc ()
  "Run insect calculator."
  (interactive)
  (shell-command (format "insect \"%s\"" (read-string "insect: "))))
