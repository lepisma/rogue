;;; packages.el --- rogue Layer packages File for Spacemacs

(setq rogue-packages
      '(multiple-cursors
        request-deferred
        svg-clock
        (blackbird :location (recipe
                              :fetcher github
                              :repo "lepisma/blackbird.el"))
        cricbuzz
        org-journal
        vue-mode
        solarized-theme
        molokai-theme
        all-the-icons
        org-sync
        enlive
        vlf
        hackernews
        swiper
        paredit
        (markup :location (recipe
                           :fetcher github
                           :repo "lepisma/markup.el"))))

;; Initialize packages
(defun rogue/init-multiple-cursors ()
  (use-package multiple-cursors
    :defer t))

(defun rogue/init-request-deferred ()
  (use-package request-deferred
    :defer t))

(defun rogue/init-svg-clock ()
  (use-package svg-clock
    :defer t))

(defun rogue/init-blackbird ()
  (use-package blackbird
    :commands (blackbird-read-lyrics)))

(defun rogue/init-cricbuzz ()
  (use-package cricbuzz
    :defer t))

(defun rogue/init-org-journal ()
  (use-package org-journal
    :defer t))

(defun rogue/init-vue-mode ()
  (use-package vue-mode
    :defer t))

(defun rogue/init-solarized-theme ()
  (use-package solarized-theme
    :defer t))

(defun rogue/init-molokai-theme ()
  (use-package molokai-theme
    :defer t))

(defun rogue/init-all-the-icons ()
  (use-package all-the-icons
    :defer t))

(defun rogue/init-org-sync ()
  (use-package org-sync
    :defer t))

(defun rogue/post-init-org-sync ()
  (use-package org-sync-github))

(defun rogue/init-enlive ()
  (use-package enlive))

(defun rogue/init-vlf ()
  (use-package vlf
    :defer t))

(defun rogue/init-hackernews ()
  (use-package hackernews
    :defer t))

(defun rogue/init-swiper ()
  (use-package swiper
    :defer t))

(defun rogue/init-paredit ()
  (use-package paredit
    :defer t))

(defun rogue/init-markup ()
  (use-package markup
    :defer t))
