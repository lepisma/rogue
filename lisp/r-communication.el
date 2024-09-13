;;; r-communication.el --- Communication setup -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;; Communication setup
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
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

;;; I use mu4e with offlineimap
;; Write credentials in ~/.authinfo.gpg and run `offlineimap' once to download all items in ~/Maildir.
;; Then run `mu init' with --my-address flags and then `mu index'.
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(defun r-communication/message-maildir-matches (msg rx)
  (when rx
    (if (listp rx)
        (or (r-communication/message-maildir-matches msg (car rx))
            (r-communication/message-maildir-matches msg (cdr rx)))
      (string-match rx (mu4e-message-field msg :maildir)))))

(use-package mu4e
  :ensure nil

  :custom
  (mu4e-get-mail-command "offlineimap -o")
  (mu4e-update-interval 600)
  (mu4e-headers-fields '((:human-date . 12)
                         (:flags      . 12)
                         (:from-or-to . 22)
                         (:maildir    . 15)
                         (:subject)))
  (mu4e-attachment-dir "~/Downloads/")
  (mu4e-view-show-images t)
  (mu4e-compose-signature "Abhinav Tushar\nhttps://lepisma.xyz\nSent with my mu4e")
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-use-fancy-chars t)
  (mu4e-headers-draft-mark          '("D"  . " "))
  (mu4e-headers-flagged-mark        '("F"  . " "))
  (mu4e-headers-new-mark            '("N"  . " "))
  (mu4e-headers-passed-mark         '("P"  . " "))
  (mu4e-headers-replied-mark        '("R"  . " "))
  (mu4e-headers-seen-mark           '("S"  . "  "))
  (mu4e-headers-trashed-mark        '("T"  . " "))
  (mu4e-headers-attach-mark         '("a"  . " "))
  (mu4e-headers-encrypted-mark      '("x"  . " "))
  (mu4e-headers-signed-mark         '("s"  . " "))
  (mu4e-headers-unread-mark         '("u"  . " "))
  (mu4e-headers-has-child-prefix    '("+"  . " "))
  (mu4e-headers-empty-parent-prefix '("-"  . " "))
  (mu4e-headers-first-child-prefix  '("\\" . " "))
  (mu4e-headers-duplicate-prefix    '("="  . " "))
  (mu4e-headers-default-prefix      '("|"  . " "))

  ;; TODO: Add SMTP configuration
  (mu4e-contexts (list (make-mu4e-context
                        :name "Gmail"
                        :match-func (lambda (msg)
                                      (when msg
                                        (r-communication/message-maildir-matches msg "^/Gmail")))
                        :vars `((mu4e-sent-messages-behavior . delete)
                                (mu4e-trash-folder . "/Gmail/[Gmail].Trash")
                                (mu4e-drafts-folder . "/Gmail/[Gmail].Drafts")
                                (mu4e-refile-folder . "/Gmail/[Gmail].Archive")))
                       (make-mu4e-context
                        :name "Personal"
                        :match-func (lambda (msg)
                                      (when msg
                                        (r-communication/message-maildir-matches msg "^/Personal")))
                        :vars `((mu4e-sent-messages-behavior . sent)
                                (mu4e-sent-folder . "/Personal/Sent")
                                (mu4e-trash-folder . "/Personal/Trash")
                                (mu4e-drafts-folder . "/Personal/Drafts")
                                (mu4e-refile-folder . "/Personal/Archive")))
                       (make-mu4e-context
                        :name "Carnil"
                        :match-func (lambda (msg)
                                      (when msg
                                        (r-communication/message-maildir-matches msg "^/Carnil")))
                        :vars `((mu4e-sent-messages-behavior . sent)
                                (mu4e-sent-folder . "/Carnil/Sent Messages")
                                (mu4e-trash-folder . "/Carnil/Deleted Messages")
                                (mu4e-drafts-folder . "/Carnil/Drafts")
                                (mu4e-refile-folder . "/Carnil/Archive")))))

  :config
  (setq mail-user-agent 'mu4e-user-agent
        message-kill-buffer-on-exit t
        message-citation-line-format "On %a, %d %b %Y at %R (%Z), %f wrote:\n"
        message-citation-line-function #'message-insert-formatted-citation-line
        mml-secure-openpgp-sign-with-sender t))

(provide 'r-communication)

;;; r-communication.el ends here
