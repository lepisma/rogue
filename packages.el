;;; packages.el --- rogue Layer packages File for Spacemacs

(defconst rogue-packages
  '(all-the-icons
    beacon
    bm
    browse-at-remote
    cricbuzz
    calfw
    dired-subtree
    doom-themes
    (elnode :location (recipe :fetcher github :repo "lepisma/elnode"))
    enlive
    (esi :location (recipe :fetcher github :repo "lepisma/esi"))
    flycheck-mypy
    hackernews
    helm-bm
    helm-org-rifle
    (kde :location (recipe :fetcher github :repo "lepisma/kde.el"))
    multiple-cursors
    ob-async
    (org-books :location (recipe :fetcher github :repo "lepisma/org-books"))
    (org-expand :location (recipe :fetcher github :repo "lepisma/org-expand"))
    org-gcal
    org-journal
    (org-pretty-table :location (recipe :fetcher github :repo "Fuco1/org-pretty-table"))
    pretty-mode
    (read-lyrics :location (recipe :fetcher github :repo "lepisma/read-lyrics.el"))
    snakemake-mode
    solaire-mode
    (spaceline-all-the-icons :location local)
    swiper
    vue-mode
    writegood-mode
    writeroom-mode
    wttrin))

;; Initialize packages
(defun rogue/init-all-the-icons ()
  (use-package all-the-icons))

(defun rogue/init-beacon ()
  :config
  (beacon-mode +1)
  (setq beacon-color (face-attribute 'region :background nil t)
        beacon-blink-when-buffer-changes t
        beacon-blink-when-point-moves-vertically 10))

(defun rogue/init-bm ()
  (use-package bm
    :demand t
    :init
    (setq bm-restore-repository-on-load t)
    (setq bm-repository-file "~/.emacs.d/.cache/bm-repository")
    :config
    (setq bm-cycle-all-buffers t)
    (setq-default bm-buffer-persistence t)
    (add-hook 'after-init-hook 'bm-repository-load)
    (add-hook 'kill-buffer-hook 'bm-buffer-save)
    (add-hook 'kill-emacs-hook (lambda ()
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))
    (add-hook 'after-save-hook 'bm-buffer-save)
    (add-hook 'find-file-hook 'bm-buffer-restore)
    (add-hook 'after-revert-hook 'bm-buffer-restore)
    (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
    :bind (("C-c b t" . bm-toggle)
           ("C-c b <right>" . bm-next)
           ("C-c b <left>" . bm-previous))))

(defun rogue/init-browse-at-remote ()
  (use-package browse-at-remote
    :defer t))

(defun rogue/init-cricbuzz ()
  (use-package cricbuzz
    :defer t))

(defun rogue/init-calfw ()
  (use-package calfw
    :after org-gcal
    :bind (("C-c q" . cfw:open-org-calendar))
    :config
    (setq cfw:fchar-junction ?┼
          cfw:fchar-vertical-line ?│
          cfw:fchar-horizontal-line ?─
          cfw:fchar-left-junction ?├
          cfw:fchar-right-junction ?┤
          cfw:fchar-top-junction ?┬
          cfw:fchar-top-left-corner ?┌
          cfw:fchar-top-right-corner ?┐)
    (require 'calfw-org)
    (setq cfw:render-line-breaker 'cfw:render-line-breaker-none)
    (setq cfw:face-item-separator-color nil)
    (setq cfw:org-face-agenda-item-foreground-color "#f92672")))

(defun rogue/init-dired-subtree ()
  (use-package dired-subtree :ensure t
    :after ranger
    :config
    (bind-key "<tab>" 'dired-subtree-toggle ranger-mode-map)
    (bind-key "<backtab>" 'dired-subtree-cycle ranger-mode-map)))

(defun rogue/init-doom-themes ()
  (use-package doom-themes
    :config
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)

    (doom-themes-neotree-config)
    (setq doom-neotree-enable-variable-pitch t
          doom-neotree-file-icons 'simple
          doom-neotree-line-spacing 4)))

(defun rogue/init-elnode ()
  (use-package elnode))

(defun rogue/init-enlive ()
  (use-package enlive))

(defun rogue/init-esi ()
  (use-package esi
    :config
    (setq esi-bm-file user-bookmarks-file)
    (setq esi-music-directory (file-name-as-directory "~/Desktop"))
    (esi-start)))

(defun rogue/init-flycheck-mypy ()
  (use-package flycheck-mypy))

(defun rogue/init-hackernews ()
  (use-package hackernews
    :bind ("C-c h" . hackernews)))

(defun rogue/init-helm-bm ()
  (use-package helm-bm
    :after bm
    :defer t))

(defun rogue/init-helm-org-rifle ()
  (use-package helm-org-rifle
    :bind ("C-c r" . helm-org-rifle-agenda-files)))

(defun rogue/init-kde ()
  (use-package kde
    :init (spacemacs/set-leader-keys
            "Ka" 'kde-kalarm-set-org
            "Ks" 'kde-kmail-send-default
            "Kx" 'kde-explore)))

(defun rogue/init-multiple-cursors ()
  (use-package multiple-cursors
    :bind (("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)
           ("C-M-<mouse-1>" . mc/add-cursor-on-click))))

(defun rogue/init-ob-async ()
  (use-package ob-async
    :config
    (add-to-list 'org-ctrl-c-ctrl-c-hook 'ob-async-org-babel-execute-src-block)))

(defun rogue/init-org-books ()
  (use-package org-books
    :config
    (setq org-books-file user-books-file)))

(defun rogue/init-org-expand ()
  (use-package org-expand
    :bind (("C-c x" . helm-org-expand))))

(defun rogue/init-org-gcal ()
  (use-package org-gcal
    :ensure t
    :after org
    :config
	  (setq org-gcal-file-alist `(("abhinav.tushar.vs@gmail.com" . ,user-gcal-file)))
    ;; Secret file
    ;; (setq org-gcal-client-id "<>"
	  ;;       org-gcal-client-secret "<>")
    (load-file (concat user-secrets-dir "gcal.el"))
    (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))
    (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))
    (add-hook 'cfw:calendar-mode-hook (lambda () (org-gcal-sync)))
    ;; (run-at-time "30 min" 1800 'org-gcal-refresh-token)
    ))

(defun rogue/init-org-journal ()
  (use-package org-journal
    :config
    (setq org-journal-dir user-diary-dir)
    (setq org-journal-enable-encryption t)))

(defun rogue/init-org-pretty-table ()
  (use-package org-pretty-table
    :demand t
    :config
    (add-hook 'org-mode-hook (lambda () (org-pretty-table-mode 1)))))

(defun rogue/init-pretty-mode ()
  (use-package pretty-mode
    :config
    (global-pretty-mode t)
    (global-prettify-symbols-mode 1)

    (pretty-deactivate-groups
     '(:equality
       :ordering
       :ordering-double
       :ordering-triple
       :arrows
       :arrows-twoheaded
       :punctuation
       :logic
       :sets
       :sub-and-superscripts
       :subscripts
       :arithmetic-double
       :arithmetic-triple))

    (pretty-activate-groups
     '(:greek :arithmetic-nary))))

(defun rogue/init-read-lyrics ()
  (use-package read-lyrics
    :commands (read-lyrics-read-lyrics)))

(defun rogue/init-snakemake-mode ()
  (use-package snakemake-mode
    :defer t))

(defun rogue/init-solaire-mode ()
  (use-package solaire-mode
    :config
    (add-hook 'prog-mode-hook 'turn-on-solaire-mode)
    (add-hook 'minibuffer-setup-hook 'solaire-mode-in-minibuffer)
    (add-hook 'ediff-prepare-buffer-hook 'solaire-mode)))

(defun rogue/init-spaceline-all-the-icons ()
  (progn
    (use-package spaceline-all-the-icons
      :after spaceline)
    (use-package spaceline
      :after powerline
      :config (setq-default mode-line-format '("%e" (:eval (spaceline-ml-ati)))))))

(defun rogue/init-swiper ()
  (use-package swiper
    :bind (("C-s" . swiper)
           ("C-r" . swiper))))

(defun rogue/init-vue-mode ()
  (use-package vue-mode
    :mode ("\\.vue\\'" . vue-mode)))

(defun rogue/init-writegood-mode ()
  (use-package writegood-mode
    :defer t))

(defun rogue/init-writeroom-mode ()
  (use-package writeroom-mode
    :defer t
    :config
    (setq writeroom-width 140
          writeroom-mode-line nil
          writeroom-global-effects '(writeroom-set-bottom-divider-width
                                     writeroom-set-internal-border-width
                                     (lambda (arg)
                                       (cond
                                        ((= arg 1)
                                         (progn (rogue-light)
                                                (setq org-src-block-faces
                                                      '(("python" (:family "Source Code Pro"
                                                                           :height 0.8))))
                                                (normal-mode)
                                                (variable-pitch-mode)))
                                        ((= arg -1)
                                         (progn (rogue-dark)
                                                (setq org-src-block-faces
                                                      '(("python" (:family "Source Code Pro"
                                                                           :height 1.0))))
                                                (normal-mode)
                                                (variable-pitch-mode)
                                                (variable-pitch-mode)))))))))

(defun rogue/init-wttrin ()
  (use-package wttrin
    :defer t
    :config
    (setq wttrin-default-cities '("Amherst?m" "Varanasi?m"))))
