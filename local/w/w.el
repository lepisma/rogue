;;; w.el --- Simple web server launcher -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25") (helm "2.9.2"))
;; URL: https://github.com/lepisma/w.el

;;; Commentary:

;; Simple web server launcher
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

(require 'helm)

(defcustom w-start-port 8080
  "First port to check for")

(defvar w-instances '()
  "List of instances currently active")

(defun w-port-active-p (port)
  "Check if a port is getting used"
  (let ((cmd (format "ss -tl4 '( sport = :%s )' | grep LISTEN"
                     (number-to-string port))))
    (> (length (shell-command-to-string cmd)) 0)))

(defun w-get-free-port (&optional start)
  "Return a free port"
  (let ((port (or start w-start-port)))
    (if (w-port-active-p port)
        (w-get-free-port (+ 1 port))
      port)))

(defun w-launch-live-server (dir port)
  "Launch live server and return process"
  (let ((default-directory dir)
        (port-arg (format "--port=%s" port)))
    (start-process "live-server" nil "live-server" port-arg "--no-browser")))

(defclass w ()
  ((dir :initarg :dir
        :documentation "Directory for the process")
   (port :initarg :port
         :documentation "Port the server is running on")
   (process :initarg :process
            :initform nil
            :documentation "Holder for the running process"))
  "A single process with info")

(cl-defmethod w-kill ((wi w))
  "Kill the connected process"
  (let ((process (oref wi process)))
    (if (process-live-p process)
        (progn
          (kill-process process)
          (setq w-instances (delete wi w-instances))))))

(cl-defmethod w-browse ((wi w))
  "Run browser for the process"
  (browse-url (format "http://localhost:%s" (oref wi port))))

(cl-defmethod w-pp ((wi w))
  "Pretty print process"
  (format "[%s] live-server: %s" (oref wi port) (abbreviate-file-name (oref wi dir))))

(defun w-dir-live-p (dir)
  "Tell which instance is serving DIR"
  (find dir w-instances :key (lambda (wi) (oref wi :dir))))

(defun w-start (&optional dir)
  "Start a new w instance in DIR"
  (interactive)
  (let* ((dir (or dir default-directory))
         (wi (w-dir-live-p dir)))
    (if wi (w-browse wi)
      (let* ((port (w-get-free-port))
             (process (w-launch-live-server dir port))
             (wi (w :dir dir :port port :process process)))
        (setq w-instances (cons wi w-instances))
        (w-browse wi)))))

(defun w-browser ()
  "Start browser for a w instance"
  (interactive)
  (let ((n-instances (length w-instances)))
    (cond ((= n-instances 0) (message "No w instances running"))
          ((= n-instances 1) (w-browse (car w-instances)))
          (t (helm :sources (helm-build-sync-source "Running w instances"
                              :candidates (mapcar (lambda (wi) (cons (w-pp wi) wi)) w-instances)
                              :action 'w-browse)
                   :buffer "*helm w browse*")))))

(defun w-stop ()
  "Stop a w instance"
  (interactive)
  (let ((n-instances (length w-instances)))
    (cond ((= n-instances 0) (message "No w instances running"))
          ((= n-instances 1) (w-kill (car w-instances)))
          (t (helm :sources (helm-build-sync-source "Running w instances"
                              :candidates (mapcar (lambda (wi) (cons (w-pp wi) wi)) w-instances)
                              :action 'w-kill)
                   :buffer "*helm w stop*")))))

(provide 'w)

;;; w.el ends here
