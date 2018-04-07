;;; rogue-mu4e.el --- mu4e config for rogue layer

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/lepisma/rogue/tree/master/local/rogue-mu4e

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
(require 'mml)
(require 'message)
(require 'mu4e)
(require 'authinfo)
(require 'openwith)
(require 's)

(defun rogue-mu4e-unread-bm-query ()
  "Return query string for unread bookmark"
  (let ((bm-item (car
                  (member-if (lambda (bm)
                               (string-equal "All Unread"
                                             (cl-struct-slot-value 'mu4e-bookmark 'name bm))) mu4e-bookmarks))))
    (cl-struct-slot-value 'mu4e-bookmark 'query bm-item)))

(defun rogue-mu4e-get-unread-mails ()
  "Return unread emails"
  (let ((cmd-out (shell-command-to-string (concat "mu find --format=sexp " (rogue-mu4e-unread-bm-query)))))
    (if (s-starts-with-p "mu: no matches for" cmd-out) nil
      (nreverse (car (read-from-string (concat "(" cmd-out ")")))))))

(defun rogue-mu4e-sign-and-send ()
  "Sign and send message"
  (interactive)
  (let ((ow-state (bound-and-true-p openwith-mode)))
    (openwith-mode -1)
    (mml-secure-sign)
    (message-send-and-exit)
    (openwith-mode (if ow-state 1 -1))))

(defun rogue-mu4e--message-maildir-matches (msg rx)
  (when rx
    (if (listp rx)
        ;; if rx is a list, try each one for a match
        (or (rogue-mu4e--message-maildir-matches msg (car rx))
            (rogue-mu4e--message-maildir-matches msg (cdr rx)))
      ;; not a list, check rx
      (string-match rx (mu4e-message-field msg :maildir)))))

(defun rogue-mu4e-setup ()
  "Setup everything."

  (setq mu4e-get-mail-command "offlineimap -o"
        mu4e-update-interval 3600
        message-kill-buffer-on-exit t
        mu4e-headers-fields '((:human-date . 12)
                              (:flags      . 12)
                              (:from-or-to . 22)
                              (:maildir    . 15)
                              (:subject))
        mu4e-attachment-dir "~/Downloads/"
        mu4e-view-show-images t)

  (setq mu4e-use-fancy-chars t
        mu4e-headers-draft-mark          '("D"  . " ")
        mu4e-headers-flagged-mark        '("F"  . " ")
        mu4e-headers-new-mark            '("N"  . " ")
        mu4e-headers-passed-mark         '("P"  . " ")
        mu4e-headers-replied-mark        '("R"  . " ")
        mu4e-headers-seen-mark           '("S"  . ""  )
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
                              :name "Unified Inbox"
                              :query (concat "maildir:/Gmail/INBOX OR "
                                             "maildir:/UMassCS/INBOX OR "
                                             "maildir:/UMass/INBOX OR "
                                             "maildir:/Fastmail/INBOX")
                              :key ?i)
                             (make-mu4e-bookmark
                              :name "All Sent"
                              :query (concat "\"maildir:/Gmail/[Gmail].Sent Mail\" OR "
                                             "maildir:/UMassCS/Sent OR "
                                             "maildir:/UMass/INBOX.Sent OR "
                                             "maildir:/Fastmail/Sent")
                              :key ?s)
                             (make-mu4e-bookmark
                              :name "All Unread"
                              :query (concat "maildir:/Gmail/INBOX AND flag:unread OR "
                                             "maildir:/UMassCS/INBOX AND flag:unread OR "
                                             "maildir:/UMass/INBOX AND flag:unread OR "
                                             "maildir:/Fastmail/INBOX AND flag:unread")
                              :key ?u)
                             (make-mu4e-bookmark
                              :name "Archived"
                              :query (concat "maildir:/Gmail/[Gmail].Archive OR "
                                             "maildir:/UMassCS/Archive OR "
                                             "maildir:/UMass/INBOX.Archive OR "
                                             "maildir:/Fastmail/Archive")
                              :key ?a))
        mu4e-contexts (list (make-mu4e-context
                             :name "Gmail"
                             :match-func (lambda (msg) (when msg (rogue-mu4e--message-maildir-matches msg "^/Gmail")))
                             :vars `((user-mail-address . ,(authinfo-get-value "imap.gmail.com" "993" "email"))
                                     (smtpmail-default-smtp-server . "smtp.gmail.com")
                                     (smtpmail-smtp-server . "smtp.gmail.com")
                                     (smtpmail-smtp-service . 465)
                                     (smtpmail-stream-type . ssl)
                                     (smtpmail-smtp-user . ,(authinfo-get-value "smtp.gmail.com" "465" "login"))
                                     ;; Gmail handles sent mails automatically
                                     (mu4e-sent-messages-behavior . delete)
                                     (mu4e-trash-folder . "/Gmail/[Gmail].Trash")
                                     (mu4e-drafts-folder . "/Gmail/[Gmail].Drafts")
                                     (mu4e-refile-folder . "/Gmail/[Gmail].Archive")))
                            (make-mu4e-context
                             :name "CSUMass"
                             :match-func (lambda (msg) (when msg (rogue-mu4e--message-maildir-matches msg "^/UMassCS")))
                             :vars `((user-mail-address . ,(authinfo-get-value "mailsrv.cs.umass.edu" "993" "email"))
                                     (smtpmail-default-smtp-server . "mailsrv.cs.umass.edu")
                                     (smtpmail-smtp-server . "mailsrv.cs.umass.edu")
                                     (smtpmail-smtp-service . 465)
                                     (smtpmail-stream-type . ssl)
                                     (smtpmail-smtp-user . ,(authinfo-get-value "mailsrv.cs.umass.edu" "465" "login"))
                                     (mu4e-sent-messages-behavior . sent)
                                     (mu4e-sent-folder . "/UMassCS/Sent")
                                     (mu4e-drafts-folder . "/UMassCS/Drafts")
                                     (mu4e-trash-folder . "/UMassCS/Trash")
                                     (mu4e-refile-folder . "/UMassCS/Archive")))
                            (make-mu4e-context
                             :name "UMass"
                             :match-func (lambda (msg) (when msg (rogue-mu4e--message-maildir-matches msg "^/UMass")))
                             :vars `((user-mail-address . ,(authinfo-get-value "mail-a.oit.umass.edu" "993" "email"))
                                     (smtpmail-default-smtp-server . "mail-auth.oit.umass.edu")
                                     (smtpmail-smtp-server . "mail-auth.oit.umass.edu")
                                     (smtpmail-smtp-service . 465)
                                     (smtpmail-stream-type . ssl)
                                     (smtpmail-smtp-user . ,(authinfo-get-value "mail-auth.oit.umass.edu" "465" "login"))
                                     (mu4e-sent-messages-behavior . sent)
                                     (mu4e-sent-folder . "/UMass/INBOX.Sent")
                                     (mu4e-drafts-folder . "/UMass/INBOX.Drafts")
                                     (mu4e-trash-folder . "/UMass/INBOX.Trash")
                                     (mu4e-refile-folder . "/UMass/INBOX.Archive")))
                            (make-mu4e-context
                             :name "Fastmail"
                             :match-func (lambda (msg) (when msg (rogue-mu4e--message-maildir-matches msg "^/Fastmail")))
                             :vars `((user-mail-address . ,(authinfo-get-value "imap.fastmail.com" "993" "email"))
                                     (smtpmail-default-smtp-server . "smtp.fastmail.com")
                                     (smtpmail-smtp-server . "smtp.fastmail.com")
                                     (smtpmail-smtp-service . 465)
                                     (smtpmail-stream-type . ssl)
                                     (smtpmail-smtp-user . ,(authinfo-get-value "smtp.fastmail.com" "465" "login"))
                                     (mu4e-sent-messages-behavior . sent)
                                     (mu4e-sent-folder . "/Fastmail/Sent")
                                     (mu4e-drafts-folder . "/Fastmail/Drafts")
                                     (mu4e-trash-folder . "/Fastmail/Trash")
                                     (mu4e-refile-folder . "/Fastmail/Archive")))))

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (add-hook 'mu4e-compose-mode-hook #'flyspell-mode))

(provide 'rogue-mu4e)

;;; rogue-mu4e.el ends here
