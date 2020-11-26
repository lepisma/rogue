;;; r-mu4e.el --- mu4e config for rogue layer

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Contains personal config for mu4e package
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-macs)
(require 'dash)
(require 'dash-functional)
(require 'helm)
(require 'mml)
(require 'message)
(require 'mu4e)
(require 'openwith)
(require 's)

(defcustom r-mu4e/named-email-ids nil
  "Pairs of aliases and private email ids."
  :type '(alist :key-type string :value-type string))

(defun authinfo-get-entries ()
  "Return entries from authinfo"
  (let* ((auth-file "~/.authinfo.gpg")
         (buffer (find-file-noselect auth-file))
         (buffer-str (with-current-buffer buffer (buffer-substring-no-properties (point-min) (point-max))))
         (entries (->> buffer-str
                     (s-split "\n")
                     (-remove (-cut s-starts-with-p "#" <>))
                     (-remove (-cut string= "" <>))
                     (-map (-cut s-split " " <>)))))
    (kill-buffer buffer)
    entries))

(defun authinfo-entry-by-name (entry-name)
  "Return matching entry for given name identifier"
  (let ((entries (authinfo-get-entries)))
    (-find (lambda (entry) (string-equal (lax-plist-get entry "name") entry-name)) entries)))

(defun authinfo-get (entry key)
  (lax-plist-get entry key))

(defun r-mu4e/get-bm-query (bm-name)
  "Return query string for unread bookmark"
  (-if-let (bm (-find (lambda (bm) (string-equal bm-name (plist-get bm :name))) mu4e-bookmarks))
      (plist-get bm :query)))

(defun r-mu4e/get-mails (query)
  "Get emails by query."
  (let ((cmd-out (shell-command-to-string (format "mu find --format=sexp \"%s\"" query))))
    (if (s-starts-with-p "mu: no matches for" cmd-out) nil
      (nreverse (car (read-from-string (concat "(" cmd-out ")")))))))

(defun r-mu4e/insert-unread-as-org-todos (bm-name buffer-days max-limit)
  "Insert unread emails as org mode style todos in current buffer."
  (erase-buffer)
  (dolist (email (reverse (r-mu4e/get-mails (r-mu4e/get-bm-query bm-name))))
    (let* ((date (plist-get email :date))
           (delta-days (floor (/ (float-time (time-subtract (current-time) date)) 60 60 24)))
           (schedule-at (time-add date (* 24 60 60 buffer-days))))
      (org-insert-heading)
      (org-insert-link nil (concat "mu4e:msgid:" (plist-get email :message-id)) (plist-get email :subject))
      (insert "\n")
      (org-schedule nil (format-time-string "%Y-%m-%d" schedule-at))
      (when (> delta-days max-limit)
        (org-priority org-priority-highest))
      (insert "\n")))
  (save-buffer))

;;;###autoload
(defun r-mu4e/send ()
  "Sign and send message"
  (interactive)
  (let ((ow-state (bound-and-true-p openwith-mode)))
    (unwind-protect
        (progn
          (openwith-mode -1)
          (when (string= (mu4e-context-name (mu4e-context-current)) "Fastmail")
            (mml-secure-sign))
          (message-send-and-exit))
      (openwith-mode (if ow-state 1 -1)))))

(defun r-mu4e/send-to-named-id ()
  "Add named email id in compose buffer. Assume cursor is at To:"
  (interactive)
  (let ((email-id (helm :sources (helm-build-sync-source "Targets"
                                       :candidates r-mu4e/named-email-ids)
                            :buffer "*helm mu4e-named-email-ids*")))
    (insert email-id)))

(defun r-mu4e//message-maildir-matches (msg rx)
  (when rx
    (if (listp rx)
        ;; if rx is a list, try each one for a match
        (or (r-mu4e//message-maildir-matches msg (car rx))
            (r-mu4e//message-maildir-matches msg (cdr rx)))
      ;; not a list, check rx
      (string-match rx (mu4e-message-field msg :maildir)))))

;;;###autoload
(defun r-mu4e/setup ()
  "Setup everything."

  (setq mu4e-get-mail-command "offlineimap -o"
        mu4e-update-interval (* 30 60)
        message-kill-buffer-on-exit t
        mu4e-headers-fields '((:human-date . 12)
                              (:flags      . 12)
                              (:from-or-to . 22)
                              (:maildir    . 15)
                              (:subject))
        mu4e-attachment-dir "~/Downloads/"
        mu4e-view-show-images t
        mu4e-compose-signature "Abhinav Tushar\nhttps://lepisma.xyz\nSent with my mu4e"
        mu4e-compose-dont-reply-to-self t
        mml-secure-openpgp-sign-with-sender t)

  (setq mu4e-use-fancy-chars t
        mu4e-headers-draft-mark          '("D"  . " ")
        mu4e-headers-flagged-mark        '("F"  . " ")
        mu4e-headers-new-mark            '("N"  . " ")
        mu4e-headers-passed-mark         '("P"  . " ")
        mu4e-headers-replied-mark        '("R"  . " ")
        mu4e-headers-seen-mark           '("S"  . "")
        mu4e-headers-trashed-mark        '("T"  . " ")
        mu4e-headers-attach-mark         '("a"  . " ")
        mu4e-headers-encrypted-mark      '("x"  . " ")
        mu4e-headers-signed-mark         '("s"  . " ")
        mu4e-headers-unread-mark         '("u"  . " ")
        mu4e-headers-has-child-prefix    '("+"  . " ")
        mu4e-headers-empty-parent-prefix '("-"  . " ")
        mu4e-headers-first-child-prefix  '("\\" . " ")
        mu4e-headers-duplicate-prefix    '("="  . " ")
        mu4e-headers-default-prefix      '("|"  . " "))

  (setq mu4e-marks
        '((refile :char ("r" . "")
                  :prompt "refile"
                  :dyn-target (lambda (target msg) (mu4e-get-refile-folder msg))
                  :action (lambda (docid msg target)
                            (mu4e~proc-move docid
                                            (mu4e~mark-check-target target)
                                            "-N")))
          (delete :char ("D" . "")
                  :prompt "Delete"
                  :show-target (lambda (target) "delete")
                  :action (lambda (docid msg target) (mu4e~proc-remove docid)))
          (flag :char ("+" . "")
                :prompt "+flag"
                :show-target (lambda (target) "flag")
                :action (lambda (docid msg target) (mu4e~proc-move docid nil "+F-u-N")))
          (move :char ("m" . "")
                :prompt "move"
                :ask-target mu4e~mark-get-move-target
                :action (lambda (docid msg target)
                          (mu4e~proc-move docid
                                          (mu4e~mark-check-target target)
                                          "-N")))
          (read :char ("!" . "")
                :prompt "!read"
                :show-target (lambda (target) "read")
                :action (lambda (docid msg target) (mu4e~proc-move docid nil "+S-u-N")))
          (trash :char ("d" . "")
                 :prompt "dtrash"
                 :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                 :action (lambda (docid msg target)
                           (mu4e~proc-move docid
                                           (mu4e~mark-check-target target)
                                           "+T-N")))
          (unflag :char ("-" . "")
                  :prompt "-unflag"
                  :show-target (lambda (target) "unflag")
                  :action (lambda (docid msg target) (mu4e~proc-move docid nil "-F-N")))
          (untrash :char ("=" . "")
                   :prompt "=untrash"
                   :show-target (lambda (target) "untrash")
                   :action (lambda (docid msg target) (mu4e~proc-move docid nil "-T")))
          (unread :char ("?" . "")
                  :prompt "?unread"
                  :show-target (lambda (target) "unread")
                  :action (lambda (docid msg target) (mu4e~proc-move docid nil "-S+u-N")))
          (unmark :char " "
                  :prompt "unmark"
                  :action (mu4e-error "No action for unmarking"))
          (action :char ("a" . "◯")
                  :prompt "action"
                  :ask-target (lambda nil (mu4e-read-option "Action: " mu4e-headers-actions))
                  :action (lambda (docid msg actionfunc)
                            (save-excursion
                              (when
                                  (mu4e~headers-goto-docid docid)
                                (mu4e-headers-action actionfunc)))))
          (something :char ("*" . "✱")
                     :prompt "*something"
                     :action (mu4e-error "No action for deferred mark"))))

  (setq mu4e-bookmarks (list (make-mu4e-bookmark
                              :name "Personal Inbox"
                              :query (concat "maildir:/Gmail/INBOX OR "
                                             "maildir:/Fastmail/INBOX")
                              :key ?i)
                             (make-mu4e-bookmark
                              :name "Work Inbox"
                              :query "maildir:/Work/INBOX"
                              :key ?w)
                             (make-mu4e-bookmark
                              :name "Work Unread"
                              :query "maildir:/Work/INBOX AND flag:unread"
                              :key ?q)
                             (make-mu4e-bookmark
                              :name "All Sent"
                              :query (concat "\"maildir:/Gmail/[Gmail].Sent Mail\" OR "
                                             "maildir:/Fastmail/Sent OR "
                                             "\"maildir:/Work/[Gmail].Sent Mail\"")
                              :key ?s)
                             (make-mu4e-bookmark
                              :name "Personal Unread"
                              :query (concat "maildir:/Gmail/INBOX AND flag:unread OR "
                                             "maildir:/Fastmail/INBOX AND flag:unread")
                              :key ?u)
                             (make-mu4e-bookmark
                              :name "Personal Archived"
                              :query (concat "maildir:/Gmail/[Gmail].Archive OR "
                                             "maildir:/Fastmail/Archive")
                              :key ?a))
        mu4e-contexts (list (let ((smtp-entry (authinfo-entry-by-name "gmail-smtp")))
                              (make-mu4e-context
                               :name "Gmail"
                               :match-func (lambda (msg) (when msg (r-mu4e//message-maildir-matches msg "^/Gmail")))
                               :vars `((user-mail-address . ,(authinfo-get smtp-entry "email"))
                                       (smtpmail-default-smtp-server . ,(authinfo-get smtp-entry "machine"))
                                       (smtpmail-smtp-server . ,(authinfo-get smtp-entry "machine"))
                                       (smtpmail-smtp-service . ,(string-to-number (authinfo-get smtp-entry "port")))
                                       (smtpmail-stream-type . ssl)
                                       (smtpmail-smtp-user . ,(authinfo-get smtp-entry "login"))
                                       ;; Gmail handles sent mails automatically
                                       (mu4e-sent-messages-behavior . delete)
                                       (mu4e-trash-folder . "/Gmail/[Gmail].Trash")
                                       (mu4e-drafts-folder . "/Gmail/[Gmail].Drafts")
                                       (mu4e-refile-folder . "/Gmail/[Gmail].Archive"))))
                            (let ((smtp-entry (authinfo-entry-by-name "fastmail-smtp")))
                              (make-mu4e-context
                               :name "Fastmail"
                               :match-func (lambda (msg) (when msg (r-mu4e//message-maildir-matches msg "^/Fastmail")))
                               :vars `((user-mail-address . ,(authinfo-get smtp-entry "email"))
                                       (smtpmail-default-smtp-server . ,(authinfo-get smtp-entry "machine"))
                                       (smtpmail-smtp-server . ,(authinfo-get smtp-entry "machine"))
                                       (smtpmail-smtp-service . ,(string-to-number (authinfo-get smtp-entry "port")))
                                       (smtpmail-stream-type . ssl)
                                       (smtpmail-smtp-user . ,(authinfo-get smtp-entry "login"))
                                       (mu4e-sent-messages-behavior . sent)
                                       (mu4e-sent-folder . "/Fastmail/Sent")
                                       (mu4e-drafts-folder . "/Fastmail/Drafts")
                                       (mu4e-trash-folder . "/Fastmail/Trash")
                                       (mu4e-refile-folder . "/Fastmail/Archive"))))
                            (let ((smtp-entry (authinfo-entry-by-name "work-smtp")))
                              (make-mu4e-context
                               :name "Work"
                               :match-func (lambda (msg) (when msg (r-mu4e//message-maildir-matches msg "^/Work")))
                               :vars `((user-mail-address . ,(authinfo-get smtp-entry "email"))
                                       (smtpmail-default-smtp-server . ,(authinfo-get smtp-entry "machine"))
                                       (smtpmail-smtp-server . ,(authinfo-get smtp-entry "machine"))
                                       (smtpmail-smtp-service . ,(string-to-number (authinfo-get smtp-entry "port")))
                                       (smtpmail-stream-type . ssl)
                                       (smtpmail-smtp-user . ,(authinfo-get smtp-entry "login"))
                                       ;; Gmail handles sent mails automatically
                                       (mu4e-sent-messages-behavior . delete)
                                       (mu4e-trash-folder . "/Work/[Gmail].Trash")
                                       (mu4e-drafts-folder . "/Work/[Gmail].Drafts")
                                       (mu4e-refile-folder . "/Work/[Gmail].Archive"))))))

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (add-hook 'mu4e-compose-mode-hook #'flyspell-mode)
  (add-hook 'mu4e-index-updated-hook
            (lambda ()
              (with-current-buffer (find-file-noselect (concat user-notes-dir "personal/emails.org"))
                (r-mu4e/insert-unread-as-org-todos "Personal Unread" 2 7))
              (with-current-buffer (find-file-noselect (concat user-notes-dir "work/emails.org"))
                (r-mu4e/insert-unread-as-org-todos "Work Unread" 2 4)))))

(provide 'r-mu4e)

;;; r-mu4e.el ends here
