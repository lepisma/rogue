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

(defun to-fish-find-file (candidate)
  "Run find file for given bookmark"
  (helm-find-files-1 (concat (file-name-as-directory (expand-file-name "~/.tofish"))
                             candidate "/")))

(defun to-fish-jump ()
  "Jump to to-fish bookmarks"
  (interactive)
  (helm :sources (helm-build-sync-source "bookmarks"
                   :candidates (directory-files "~/.tofish")
                   :action '(("Jump to bookmark" . to-fish-find-file)))
        :buffer "*helm tofish jump*"
        :prompt "Jump to : "))

(defun git-archive (output-file)
  "Archive current repository"
  (interactive "FArchive output file (with extension): ")
  (if (magit-toplevel)
      (progn
        (call-process "git" nil nil nil "archive"
                      "-o" output-file "HEAD")
        (message "git-archive finished"))
    (message "Not in a git repository")))

(defun transform-pair-units (pairs)
  "Transform unit pairs to SI. Just temp for now."
  (mapcar
   (lambda (pair)
     (let* ((split (split-string (second pair) "°"))
            (value (string-to-number (first split)))
            (unit (second split)))
       (if (string-equal unit "F")
           `(,(first pair)
             ,(concat (format "%0.2f" (/ (- value 32) 1.8)) "°C"))
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
    (mapc (lambda (pair) (insert (concat "+ " (first pair) " :: " (second pair) "\n")))
          (transform-pair-units (butlast pairs)))
    (switch-to-buffer buffer)
    (setq buffer-read-only t)
    (goto-char (point-min))))

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
  nil nil weather-amherst-mode-map)

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

(defun delete-line ()
  "Delete current line without killing"
  (interactive)
  (delete-region (line-beginning-position) (line-end-position)))

(defun duplicate-line ()
  "Duplicate a line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank)
  (setq kill-ring (cdr kill-ring)))

(defun insect-calc ()
  "Run insect calculator."
  (interactive)
  (shell-command (format "insect \"%s\"" (read-string "insect: "))))

(defun org-shuffle-save ()
  "Shuffle and save current file"
  (interactive)
  (goto-char (point-min))
  (org-sort-entries nil ?f (lambda () (random 1000)))
  (save-buffer))

(defun org-shuffle-projects ()
  "Shuffle first level items in project files"
  (interactive)
  (dolist (project-file user-project-files)
    (find-file project-file)
    (org-shuffle-save)))

(defun mpc-send-message (channel message)
  "Send message to mpc"
  (if (eq 0 (call-process "mpc" nil nil nil "sendmessage" channel message))
      (message "Done")
    (message "Error in sending message to mpc")))

(defun mpdas-love ()
  "Love song on scrobbler service"
  (interactive)
  (mpc-send-message "mpdas" "love"))

(defun git-update-project (project-root)
  "Add all, commit and push given project."
  (let ((default-directory project-root))
    ;; Blocking add
    (shell-command-to-string "git add .")
    ;; Using this to avoid gpg tty issue + to use settings from emacs
    (ignore-errors
      (magit-commit '("-m" "git-auto-update")))
    (call-process-shell-command "git push" nil 0)))

(defun rogue-light ()
  "Switch to light theme"
  (interactive)
  (setq doom-neotree-enable-variable-pitch nil
        doom-neotree-line-spacing 1)
  (spacemacs/load-theme 'spacemacs-light)
  (setq org-bullets-bullet-list '(" "))
  (set-face-attribute 'org-indent nil
                      :inherit '(org-hide fixed-pitch)))

(defun rogue-dark ()
  "Switch to dark theme"
  (interactive)
  (setq doom-neotree-enable-variable-pitch t
        doom-neotree-line-spacing 4)
  (disable-theme 'spacemacs-light)
  (spacemacs/load-theme 'doom-molokai)
  (setq org-bullets-bullet-list '("#"))
  (set-face-attribute 'org-indent nil
                      :inherit '(org-hide)))
