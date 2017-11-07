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

(defun git-update-project (project-root)
  "Add all, commit and push given project."
  (let ((default-directory project-root))
    ;; Blocking add
    (shell-command-to-string "git add .")
    ;; Using this to avoid gpg tty issue + to use settings from emacs
    (ignore-errors
      (magit-commit '("-m" "git-auto-update")))
    (call-process-shell-command "git push" nil 0)))

(defun reset-org-buffers ()
  "Reset org-mode in all org buffers"
  (mapc (lambda (buff)
          (with-current-buffer buff
            (if (string-equal "org-mode" major-mode)
                (org-mode))))
        (buffer-list)))

(defun rogue-cycle-theme ()
  "Cycle between dark and light scheme"
  (interactive)
  (if (eq rogue-current-theme rogue-dark-theme)
      (progn
        (rogue-light)
        (setq rogue-current-theme rogue-light-theme))
    (progn
      (rogue-dark)
      (setq rogue-current-theme rogue-dark-theme))))

(defun rogue-light ()
  "Switch to light theme"
  (interactive)
  (setq doom-neotree-enable-variable-pitch nil
        doom-neotree-line-spacing 1)
  (disable-theme rogue-dark-theme)
  (spacemacs/load-theme rogue-light-theme)
  (setq org-bullets-bullet-list '(" "))
  (reset-org-buffers)
  (beacon-mode -1))

(defun rogue-dark ()
  "Switch to dark theme"
  (interactive)
  (setq doom-neotree-enable-variable-pitch t
        doom-neotree-line-spacing 4)
  (disable-theme rogue-light-theme)
  (spacemacs/load-theme rogue-dark-theme)
  (setq org-bullets-bullet-list '("#"))
  (reset-org-buffers)
  (beacon-mode +1))

(defun mu4e-unread-bm-query ()
  "Return query string for unread bookmark"
  (let ((bm-item (car
                  (member-if (lambda (bm)
                               (string-equal "All Unread"
                                             (cl-struct-slot-value 'mu4e-bookmark 'name bm))) mu4e-bookmarks))))
    (cl-struct-slot-value 'mu4e-bookmark 'query bm-item)))

(defun mu4e-get-unread-mails ()
  "Return unread emails"
  (let ((cmd-out (shell-command-to-string (concat "mu find --format=sexp " (mu4e-unread-bm-query)))))
    (nreverse (car (read-from-string (concat "(" cmd-out ")"))))))

(defun quack-quack (text)
  "Speak the given text"
  (start-process "quack" nil "quack" text))

(defun quack-unread-mail ()
  "Read unread emails"
  (interactive)
  (let ((subjects (mapcar
                   (lambda (mail) (plist-get mail :subject))
                   (mu4e-get-unread-mails))))
    (quack-quack (format "You have %s. %s"
                         (cond ((= (length subjects) 0) "no unread emails")
                               ((= (length subjects) 1) "1 unread email")
                               (t (format "%s unread emails" (length subjects))))
                         (s-join ". " subjects)))))

(defun message-sign-and-send ()
  "Sign and send message"
  (interactive)
  (mml-secure-sign)
  (message-send-and-exit))

(defun color-buffer-text (text color)
  "Color the text in current buffer."
  (goto-char (point-min))
  (while (search-forward text nil t)
    (put-text-property (- (point) (length text)) (point)
                       'font-lock-face `(:background ,color))))

(defun get-buffer-numbers ()
  "Get a list of all numbers in current buffer."
  (let ((text (s-collapse-whitespace (substring-no-properties (buffer-string)))))
    (cl-remove-if #'zerop (mapcar #'string-to-number (s-split "," (s-replace " " "," text))))))

(defun prodigy-define-basic (name &optional args)
  (prodigy-define-service
    :name name
    :command name
    :args args
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t))

(defun org-clock-in-default ()
  "Default clock in clock.org"
  (interactive)
  (with-current-buffer "clock.org"
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (org-clock-in)
    (org-save-all-org-buffers)))

(defun org-clock-out-default ()
  "Default clock in clock.org"
  (interactive)
  (with-current-buffer "clock.org"
    (org-clock-out)
    (org-save-all-org-buffers)))
