;;; packages.el --- rogue Layer packages File for Spacemacs

(setq rogue-packages
      '(all-the-icons
        bm
        browse-at-remote
        cricbuzz
        enlive
        ;; (flycheck-coala :location (recipe
        ;;                            :fetcher github
        ;;                            :repo "coala/coala-emacs"))
        flycheck-mypy
        hackernews
        helm-bm
        magithub
        molokai-theme
        multiple-cursors
        (ob-async :location (recipe
                             :fetcher github
                             :repo "astahlman/ob-async"))
        (ob-q :location (recipe
                         :fetcher github
                         :repo "lepisma/ob-q.el"))
        org-journal
        paredit
        (read-lyrics :location (recipe
                                :fetcher github
                                :repo "lepisma/read-lyrics.el"))
        snakemake-mode
        solarized-theme
        ;; (spaceline-all-the-icons :location local)
        swiper
        tide
        tldr
        vlf
        vue-mode
        wolfram
        writegood-mode
        writeroom-mode
        wttrin))

;; Initialize packages

(defun rogue/init-all-the-icons ()
  (use-package all-the-icons
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

(defun rogue/init-browse-at-remote ()
  (use-package browse-at-remote
    :defer t))

(defun rogue/init-cricbuzz ()
  (use-package cricbuzz
    :defer t))

(defun rogue/init-enlive ()
  (use-package enlive))

;; (defun rogue/init-flycheck-coala ()
;;   (use-package flycheck-coala))

(defun rogue/init-flycheck-mypy ()
  (use-package flycheck-mypy))

(defun rogue/init-hackernews ()
  (use-package hackernews
    :defer t
    :bind ("C-c h" . hackernews)))

(defun rogue/init-helm-bm ()
  (use-package helm-bm
    :defer t))

(defun rogue/init-magithub ()
  (use-package magithub
    :after magit
    :config (magithub-feature-autoinject t)))

(defun rogue/init-molokai-theme ()
  (use-package molokai-theme
    :defer t))

(defun rogue/init-multiple-cursors ()
  (use-package multiple-cursors
    :defer t
    :bind (("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this))))

(defun rogue/init-ob-async ()
  (use-package ob-async
    :config (add-to-list
             'org-ctrl-c-ctrl-c-hook
             'org-babel-execute-src-block:async)))

(defun rogue/init-ob-q ()
  (use-package ob-q
    :defer t))

(defun rogue/init-org-journal ()
  (use-package org-journal
    :defer t))

(defun rogue/init-paredit ()
  (use-package paredit
    :defer t))

(defun rogue/init-read-lyrics ()
  (use-package read-lyrics
    :commands (read-lyrics-read-lyrics)))

(defun rogue/init-snakemake-mode ()
  (use-package snakemake-mode
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

;; (defun rogue/init-spaceline-all-the-icons ()
;;   (progn
;;     (use-package spaceline-all-the-icons
;;       :after spaceline)
;;     (use-package spaceline
;;       :after powerline
;;       :config (setq-default mode-line-format '("%e" (:eval (spaceline-ml-ati)))))))

(defun rogue/init-swiper ()
  (use-package swiper
    :defer t
    :bind (("C-s" . swiper)
           ("C-r" . swiper))))

(defun rogue/init-tide ()
  (use-package tide
    :defer t))

(defun rogue/init-tldr ()
  (use-package tldr
    :defer t))

(defun rogue/init-vlf ()
  (use-package vlf
    :defer t))

(defun rogue/init-vue-mode ()
  (use-package vue-mode
    :mode ("\\.vue\\'" . vue-mode)))

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


(defun rogue/init-writegood-mode ()
  (use-package writegood-mode
    :defer t))

(defun rogue/init-writeroom-mode ()
  (use-package writeroom-mode
    :defer t))

(defun rogue/init-wttrin ()
  (use-package wttrin
    :defer t
    :config
    (setq wttrin-default-cities '("Amherst?m" "Varanasi?m"))))
