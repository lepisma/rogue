;;; packages.el --- rogue Layer packages File for Spacemacs

(setq rogue-packages
      '(multiple-cursors
        (blackbird :location (recipe
                              :fetcher github
                              :repo "lepisma/blackbird.el"))
        cricbuzz
        org-journal
        vue-mode
        solarized-theme
        molokai-theme
        all-the-icons
        enlive
        vlf
        hackernews
        swiper
        paredit
        (ob-q :location (recipe
                         :fetcher github
                         :repo "lepisma/ob-q.el"))
        (u :location (recipe
                      :fetcher github
                      :repo "lepisma/u"
                      :branch "elisp"))
        writegood-mode
        writeroom-mode
        snakemake-mode
        bm
        helm-bm
        tide
        symon
        god-mode
        tldr
        (spaceline-all-the-icons :location local)
        ox-ioslide
        wttrin
        wolfram))

;; Initialize packages
(defun rogue/init-multiple-cursors ()
  (use-package multiple-cursors
    :defer t
    :bind (("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this))))

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
    :defer t
    :init
    (progn
      (setq x-underline-at-descent-line t)
      (setq solarized-high-contrast-mode-line t)
      (setq solarized-use-more-italic t)
      (setq solarized-emphasize-indicators t)
      (setq solarized-scale-org-headlines nil))))

(defun rogue/init-molokai-theme ()
  (use-package molokai-theme
    :defer t))

(defun rogue/init-all-the-icons ()
  (use-package all-the-icons
    :defer t))

(defun rogue/init-enlive ()
  (use-package enlive))

(defun rogue/init-vlf ()
  (use-package vlf
    :defer t))

(defun rogue/init-hackernews ()
  (use-package hackernews
    :defer t
    :bind ("C-c h" . hackernews)))

(defun rogue/init-swiper ()
  (use-package swiper
    :defer t
    :bind (("C-s" . swiper)
           ("C-r" . swiper))))

(defun rogue/init-paredit ()
  (use-package paredit
    :defer t))

(defun rogue/init-ob-q ()
  (use-package ob-q
    :defer t))

(defun rogue/init-u ()
  (use-package u
    :defer t))

(defun rogue/init-writegood-mode ()
  (use-package writegood-mode
    :defer t))

(defun rogue/init-writeroom-mode ()
  (use-package writeroom-mode
    :defer t))

(defun rogue/init-snakemake-mode ()
  (use-package snakemake-mode
    :defer t))

(defun rogue/init-bm ()
  (use-package bm
    :defer t

    :init
    ;; restore on load (even before you require bm)
    (setq bm-restore-repository-on-load t)

    :config
    ;; Allow cross-buffer 'next'
    (setq bm-cycle-all-buffers t)

    ;; where to store persistant files
    (setq bm-repository-file "~/.emacs.d/bm-repository")

    ;; save bookmarks
    (setq-default bm-buffer-persistence t)

    ;; Loading the repository from file when on start up.
    (add-hook' after-init-hook 'bm-repository-load)

    ;; Restoring bookmarks when on file find.
    (add-hook 'find-file-hooks 'bm-buffer-restore)

    ;; Saving bookmarks
    (add-hook 'kill-buffer-hook #'bm-buffer-save)

    ;; Saving the repository to file when on exit.
    ;; kill-buffer-hook is not called when Emacs is killed, so we
    ;; must save all bookmarks first.
    (add-hook 'kill-emacs-hook #'(lambda nil
                                   (bm-buffer-save-all)
                                   (bm-repository-save)))

    ;; The `after-save-hook' is not necessary to use to achieve persistence,
    ;; but it makes the bookmark data in repository more in sync with the file
    ;; state.
    (add-hook 'after-save-hook #'bm-buffer-save)

    ;; Restoring bookmarks
    (add-hook 'find-file-hooks   #'bm-buffer-restore)
    (add-hook 'after-revert-hook #'bm-buffer-restore)

    ;; The `after-revert-hook' is not necessary to use to achieve persistence,
    ;; but it makes the bookmark data in repository more in sync with the file
    ;; state. This hook might cause trouble when using packages
    ;; that automatically reverts the buffer (like vc after a check-in).
    ;; This can easily be avoided if the package provides a hook that is
    ;; called before the buffer is reverted (like `vc-before-checkin-hook').
    ;; Then new bookmarks can be saved before the buffer is reverted.
    ;; Make sure bookmarks is saved before check-in (and revert-buffer)
    (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

    :bind ("C-c b" . bm-toggle)))

(defun rogue/init-helm-bm ()
  (use-package helm-bm
    :defer t))

(defun rogue/init-tide ()
  (use-package tide
    :defer t))

(defun rogue/init-symon ()
  (use-package symon
    :defer t))

(defun rogue/init-god-mode ()
  (use-package god-mode
    :defer t
    :bind (("<escape>" . god-mode-all)
           ("C-x C-1" . delete-other-windows)
           ("C-x C-2" . split-window-below)
           ("C-x C-3" . split-window-right)
           ("C-x C-0" . delete-window)
           :map god-local-mode-map
           ("i" . god-mode-all))
    :config
    (progn
      (defun god-update-cursor ()
        (set-cursor-color (if (or god-local-mode buffer-read-only)
                              "orange"
                            "#839496")))

      (add-hook 'god-mode-enabled-hook 'god-update-cursor)
      (add-hook 'god-mode-disabled-hook 'god-update-cursor))))

(defun rogue/init-tldr ()
  (use-package tldr
    :defer t))

(defun rogue/init-ox-ioslide ()
  (use-package ox-ioslide
    :defer t))

(defun rogue/init-wttrin ()
  (use-package wttrin
    :defer t
    :config
    (setq wttrin-default-cities '("Amherst?m" "Varanasi?m"))))

(defun rogue/init-wolfram ()
  (use-package wolfram
    :defer t
    :config
    (progn
      (require 'json)
      (let* ((json-object-type 'hash-table)
             (secrets (json-read-file user-secrets-path)))
        (setq wolfram-alpha-app-id
              (gethash "wolfram-alpha-app-id" secrets))))))

;; (defun rogue/init-spaceline-all-the-icons ()
;;   (progn
;;     (use-package spaceline-all-the-icons
;;       :after spaceline)
;;     (use-package spaceline
;;       :after powerline
;;       :config (setq-default mode-line-format '("%e" (:eval (spaceline-ml-ati)))))))
