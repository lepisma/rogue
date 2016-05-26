;;; blackbird.el --- Basic interactions with blackbird in emacs

;; Copyright (c) 2016 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav.tushar.vs@gmail.com>
;; Version: 1.0
;; Package-Requires ((request "20160108.33"))
;; Keywords: lyrics
;; URL: https://github.com/lepisma/blackbird

;;; Commentary:

;; blackbird.el provides interacting functions for blackbird.
;; Needs w3m to be installed for reading lyrics.
;; Lyrics feature forked from emms-get-lyrics.el
;; Run blackbird-read-lyrics while blackbird is running.

;;; Code:

(require 'request)

;;;###autoload
(defun blackbird-read-lyrics ()
  "Return details about current song. Assume default port number."
  (interactive)
  (request
    "http://localhost:1234/current"
    :parser 'json-read
    :timeout 5
    :success (function*
              (lambda (&key data &allow-other-keys)
                (let* ((title (assoc-default 'title data))
                       (artist (assoc-default 'artist data)))
                  (blackbird-lyrics artist title))))))

(defun blackbird-lyrics-mode ()
  "Major mode for displaying lyrics."
  (kill-all-local-variables)
  (setq major-mode 'blackbird-lyrics-mode)
  (setq mode-name "Lyrics")
  (setq buffer-read-only t)
  (run-hooks 'blackbird-lyrics-mode-hook))

(defun blackbird-lyrics-url (artist title)
  (concat
   "http://www.google.com/search?btnI&q=" (base64-decode-string "c2l0ZTplbHlyaWNzLm5ldCs=")
   (replace-regexp-in-string
    " " "+"
    (concat
     artist
     " "
     title ""))
   ""))

(defun blackbird-lyrics-w3m (url buffer)
  (call-process "w3m" nil buffer nil "-dump" url))

(defun blackbird-lyrics (artist title)
  "Display lyrics for given song"
  (let ((bname (concat "Lyrics: " title " by " artist)))
    (cond ((get-buffer bname)
           (switch-to-buffer bname))
          (t
           (let ((buffer (get-buffer-create bname)))
             (set-buffer buffer)
             (blackbird-lyrics-w3m (blackbird-lyrics-url artist title) buffer)
             (goto-char (point-min))
             (if (and
                  (search-forward "Rating" nil t)
                  (not (search-forward "No results." nil t)))
                 (let ((frominsert ""))
                   (forward-line 22)
                   (delete-region (point-min) (1+ (line-end-position)))
                   (goto-char (point-max))
                   (if (search-backward "Correct these lyrics" nil t)
                       (progn (beginning-of-line)(forward-line -1)(delete-region (point) (point-max)))))
               (delete-region (point-min) (point-max))
               (insert "Unable to find lyrics for " title " by " artist))

             (goto-char (point-min))
             (blackbird-lyrics-mode)
             (switch-to-buffer buffer)
             (goto-char (point-min)))))))

(provide 'blackbird)

;;; blackbird.el ends here
