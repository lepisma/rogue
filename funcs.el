;;; funcs.el --- rogue Layer utility functions -*- lexical-binding: t -*-

(defun to-fish-find-file (candidate)
  "Run find file for given bookmark"
  (helm-find-files-1 (file-name-as-directory (f-canonical (f-join "~/.tofish" candidate)))))

(defun dir-items (dir)
  (set-difference (directory-files dir) '("." "..") :test 'equal))

(defun to-fish-jump ()
  "Jump to to-fish bookmarks"
  (interactive)
  (helm :sources (helm-build-sync-source "bookmarks"
                   :candidates (dir-items "~/.tofish")
                   :action '(("Jump to bookmark" . to-fish-find-file)))
        :buffer "*helm tofish jump*"
        :prompt "Jump to: "))

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

(defun r/cycle-theme ()
  "Cycle between dark and light scheme"
  (interactive)
  (if (eq r-current-theme r-dark-theme)
      (progn
        (r/light)
        (setq r-current-theme r-light-theme))
    (progn
      (r/dark)
      (setq r-current-theme r-dark-theme))))

(defun r/light ()
  "Switch to light theme"
  (interactive)
  (disable-theme r-dark-theme)
  (spacemacs/load-theme r-light-theme)
  (setq org-superstar-headline-bullets-list '(" "))
  (r-org/reset-buffers))

(defun r/dark ()
  "Switch to dark theme"
  (interactive)
  (disable-theme r-light-theme)
  (spacemacs/load-theme r-dark-theme)
  (setq org-superstar-headline-bullets-list '("› "))
  (r-org/reset-buffers))

(defun magit-bookmarks ()
  (interactive)
  (let ((bms `(("website" . ,(concat user-project-dir "lepisma.github.io-deploy")))))
    (helm :sources (helm-build-sync-source "magit-bookmarks"
                     :candidates bms
                     :action `(("Open magit status" . magit-status)))
          :buffer "*helm magit bookmarks*"
          :prompt "Bookmark: ")))

(defun magit-commit-generic-update ()
  (interactive)
  (magit-commit-create '("-m" "Updates")))

(with-eval-after-load 'transient
  (with-eval-after-load 'magit
      (transient-append-suffix 'magit-commit "c"
        '("g" "Commit with generic message" magit-commit-generic-update))))

(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."

  ;; Taken from here
  ;; https://www.reddit.com/r/emacs/comments/idz35e/emacs_27_can_take_svg_screenshots_of_itself/g2c2c6y/
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

(defun serve-current-buffer (&optional port)
  "Serve current buffer."
  (interactive)
  (ws-start
   (lambda (request)
     (with-slots (process headers) request
       (ws-response-header process 200 '("Content-type" . "text/html; charset=utf-8"))
       (process-send-string process
                            (let ((html-buffer (htmlize-buffer)))
                              (prog1 (with-current-buffer html-buffer (buffer-string))
                                (kill-buffer html-buffer))))))
   (or port 9010)))

(defun range (n)
  "Python like range function returning list."
  (cl-loop for i from 0 to (- n 1)
           collect i))

(defun shuffle-list (its)
  "Destructive but inefficient list shuffling."
  (cl-loop for i downfrom (- (length its) 1) to 1
           do (let ((i-val (nth i its))
                    (j (random (+ i 1))))
                (setf (nth i its) (nth j its))
                (setf (nth j its) i-val)))
  its)

(defun shuffle-org-list ()
  "Shuffle list at point."
  (interactive)
  (save-excursion
    (let ((org-list (org-list-to-lisp t)))
      (insert (org-list-to-org (cons (car org-list) (shuffle-list (cdr org-list)))))
      (org-list-repair))))
