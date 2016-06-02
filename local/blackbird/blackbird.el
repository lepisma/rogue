;;; blackbird.el --- Basic interactions with blackbird in emacs

;; Copyright (c) 2016 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav.tushar.vs@gmail.com>
;; Version: 2.0
;; Package-Requires ((request "20160108.33") (enlive "0.0.1"))
;; Keywords: lyrics
;; URL: https://github.com/lepisma/blackbird

;;; Commentary:

;; blackbird.el provides interacting functions for blackbird.
;; This file is not a part of GNU Emacs.

;;; Code:

(require 'request)
(require 'enlive)

(defvar blackbird-api-url "http://localhost:1234")
(defvar blackbird-lyrics-search-url "https://duckduckgo.com/html/?q=site%3Aazlyrics.com+")

(defun blackbird-lyrics-mode ()
  "Major mode for displaying lyrics."
  (kill-all-local-variables)
  (setq major-mode 'blackbird-lyrics-mode)
  (setq mode-name "Lyrics")
  ;; (variable-pitch-mode)
  (run-hooks 'blackbird-lyrics-mode-hook))

;;;###autoload
(defun blackbird-read-lyrics ()
  "Get current playing track information"
  (interactive)
  (request
   (concat blackbird-api-url "/current")
   :parser 'json-read
   :timeout 5
   :success (function*
             (lambda (&key data &allow-other-keys)
               (let ((title (assoc-default 'title data))
                     (artist (assoc-default 'artist data)))
                 (blackbird-lyrics-for title artist))))
   :error (function*
           (lambda (&key error-thrown &allow-other-keys)
             (message "Error contacting player api")))))

(defun blackbird-lyrics-for (title artist)
  "Show lyrics for given song"
  (let* ((search-url (blackbird-lyrics-get-search-url
                      title
                      artist))
         (search-node (enlive-fetch search-url)))
    (if search-node
        (let ((lyrics-page-url (blackbird-lyrics-parse-search
                                search-node)))
          (if lyrics-page-url
              (blackbird-lyrics-display-page lyrics-page-url)
            (message "No lyrics results found")))
      (message "Error in search"))))

(defun blackbird-lyrics-parse-search (search-node)
  "Get link to first lyrics result from given node"
  (let ((result-urls (enlive-get-elements-by-class-name
                     search-node
                     "result__url")))
    (if result-urls
        (blackbird-lyrics-get-first-lyrics-result result-urls)
      nil)))

(defun blackbird-lyrics-get-search-url (title artist)
  "Return duckduckgo search url"
  (concat
   blackbird-lyrics-search-url
   (replace-regexp-in-string
    " " "+"
    (concat artist " " title))))

(defun blackbird-lyrics-get-lyrics-url (link-node)
  "Return lyrics page link if available in the link-node"
  (let ((link (enlive-attr link-node 'href)))
    (if (string-prefix-p "http://www.azlyrics.com/lyrics/" link)
        link
      nil)))

(defun blackbird-lyrics-get-first-lyrics-result (result-urls)
  "Return first valid (lyrics) url, if any, from the search results"
  (if result-urls
      (let ((first-link (blackbird-lyrics-get-lyrics-url
                         (car result-urls))))
        (if first-link
            first-link
          (blackbird-lyrics-get-first-lyrics-result
           (cdr result-urls))))
    nil))

(defun blackbird-lyrics-display-page (lyrics-page-url)
  "Display lyrics from the page url"
  (let ((page-node (enlive-fetch lyrics-page-url)))
    (if page-node
        (let ((page-data (blackbird-lyrics-get-page-data page-node))
              (buffer (get-buffer-create "*Lyrics*")))
          (set-buffer buffer)
          (setq buffer-read-only nil)
          (erase-buffer)
          (blackbird-lyrics-mode)
          (insert "\n")
          (insert (propertize (second page-data)
                              'face '(:inherit variable-pitch
                                               :foreground "DeepSkyBlue"
                                               :height 1.6)))
          (insert "\n")
          (insert (propertize (third page-data)
                              'face '(:inherit variable-pitch
                                               :height 1.0
                                               :weight bold
                                               :foreground "gray")))
          (insert "\n\n")
          (setq text-start (point))
          (insert (propertize (first page-data)
                              'face '(:inherit variable-pitch
                                               :height 1.1
                                               :slant italic
                                               :foreground "DeepPink1")))
          (switch-to-buffer buffer)
          (add-text-properties text-start (point-max) '(line-spacing 0.4))
          (delete-trailing-whitespace)
          (setq buffer-read-only t)
          (goto-char (point-min)))
      (message "Error in fetching page"))))

(defun blackbird-lyrics-get-page-data (page-node)
  "Return information from lyrics page"
  (let ((data nil)
        (bold-headings (enlive-get-elements-by-tag-name page-node 'b)))
    (push (string-remove-suffix " LYRICS" (enlive-text
                                           (first bold-headings)))
          data)
    (push (substring (enlive-text
                      (second bold-headings))
                     1 -1)
          data)
    (let* ((text (enlive-text (sixth (enlive-query-all
                                      page-node
                                      [div.container.main-page > div > div.col-xs-12.col-lg-8.text-center > div]))))
           (notice-text-end "licensing agreement. Sorry about that.")
           (notice-index (+ (string-match
                             notice-text-end
                             text)
                            (length notice-text-end))))
      (if notice-index
          (setq text (string-trim (substring text notice-index))))
      (push text data))
    data))

(provide 'blackbird)

;;; blackbird.el ends here
