;;; funcs.el --- rogue Layer utility functions -*- lexical-binding: t -*-

(defun to-fish-find-file (candidate)
  "Run find file for given bookmark"
  (helm-find-files-1 (file-name-as-directory (f-canonical (f-join "~/.tofish" candidate)))))

(defun to-fish-jump ()
  "Jump to to-fish bookmarks"
  (interactive)
  (helm :sources (helm-build-sync-source "bookmarks"
                   :candidates (directory-files "~/.tofish")
                   :action '(("Jump to bookmark" . to-fish-find-file)))
        :buffer "*helm tofish jump*"
        :prompt "Jump to : "))

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
  (rogue-org-reset-buffers)
  (beacon-mode -1))

(defun rogue-dark ()
  "Switch to dark theme"
  (interactive)
  (setq doom-neotree-enable-variable-pitch t
        doom-neotree-line-spacing 4)
  (disable-theme rogue-light-theme)
  (spacemacs/load-theme rogue-dark-theme)
  (setq org-bullets-bullet-list '("#"))
  (rogue-org-reset-buffers)
  (beacon-mode +1))

(defun quack-quack (text)
  "Speak the given text"
  (start-process "quack" nil "quack" text))

(defun quack-unread-mail ()
  "Read unread emails"
  (interactive)
  (let ((subjects (mapcar
                   (lambda (mail) (plist-get mail :subject))
                   (rogue-mu4e-get-unread-mails))))
    (quack-quack (format "You have %s. %s"
                         (cond ((= (length subjects) 0) "no unread emails")
                               ((= (length subjects) 1) "1 unread email")
                               (t (format "%s unread emails" (length subjects))))
                         (s-join ". " subjects)))))
