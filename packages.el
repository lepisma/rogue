;;; packages.el --- rogue Layer packages File for Spacemacs

(defconst rogue-packages
  '(all-the-icons
    (authinfo :location local)
    beacon
    browse-at-remote
    cricbuzz
    calfw
    calfw-org
    (calibre :location (recipe :fetcher github :repo "lepisma/calibre.el"))
    colormaps
    conda
    dired-subtree
    doom-themes
    enlive
    (etab :location (recipe :fetcher github :repo "lepisma/etab"))
    focus
    gscholar-bibtex
    hackernews
    hy-mode
    (kindle :location local)
    (levenshtein :location (recipe :fetcher github :repo "emacsorphanage/levenshtein"))
    (mpm :location (recipe :fetcher url :url "https://raw.githubusercontent.com/lepisma/mpm/master/emacs/mpm.el"))
    multiple-cursors
    nov
    ob-async
    ob-sagemath
    openwith
    (org-bbq :location (recipe :fetcher url :url "https://raw.githubusercontent.com/lepisma/bbq/master/emacs/org-bbq.el"))
    (org-books :location (recipe :fetcher github :repo "lepisma/org-books"))
    org-cliplink
    (org-expand :location (recipe :fetcher github :repo "lepisma/org-expand"))
    org-gcal
    (org-gh :location local)
    org-journal
    (org-pretty-table :location (recipe :fetcher github :repo "Fuco1/org-pretty-table"))
    (org-make :location local)
    parinfer
    (pile :location (recipe :fetcher github :repo "lepisma/pile-wiki"))
    pretty-mode
    (read-lyrics :location (recipe :fetcher github :repo "lepisma/read-lyrics.el"))
    (rogue-ligatures :location local)
    (rogue-mu4e :location local)
    (rogue-org :location local)
    (rogue-processes :location local)
    (rogue-ui :location local)
    (rogue-utils :location local)
    sage-shell-mode
    shell-switcher
    snakemake-mode
    solaire-mode
    (spaceline-all-the-icons :location local)
    swiper
    switch-window
    (viz :location local)
    vue-mode
    (w :location (recipe :fetcher github :repo "lepisma/w.el"))
    (weather-amherst :location local)
    writegood-mode))

;; Initialize packages
(defun rogue/init-all-the-icons ()
  (use-package all-the-icons))

(defun rogue/init-authinfo ()
  (use-package authinfo
    :after (s dash-functional)))

(defun rogue/init-beacon ()
  :config
  (beacon-mode +1)
  (setq beacon-color (face-attribute 'region :background nil t)
        beacon-blink-when-buffer-changes t
        beacon-blink-when-point-moves-vertically nil))

(defun rogue/init-browse-at-remote ()
  (use-package browse-at-remote
    :defer t))

(defun rogue/init-cricbuzz ()
  (use-package cricbuzz
    :defer t))

(defun rogue/init-calfw ()
  (use-package calfw
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
    (setq cfw:render-line-breaker 'cfw:render-line-breaker-none)
    (setq cfw:face-item-separator-color nil)))

(defun rogue/init-calfw-org ()
  (use-package calfw-org
    :after calfw
    :config
    (setq cfw:org-face-agenda-item-foreground-color "#BF616A")))

(defun rogue/init-calibre ()
  (use-package calibre
    :after (s dash-functional)
    :config
    (setq calibre-root (concat user-cloud-dir "Calibre Shared"))))

(defun rogue/init-colormaps ()
  (use-package colormaps
    :defer t))

(defun rogue/init-conda ()
  (use-package conda
    :config
    (conda-env-initialize-eshell)
    (setq conda-anaconda-home (expand-file-name "~/.miniconda"))))

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
          doom-neotree-line-spacing 4)
    (doom-themes-org-config)))

(defun rogue/init-elnode ()
  (use-package elnode))

(defun rogue/init-enlive ()
  (use-package enlive))

(defun rogue/init-etab ()
  (use-package etab
    :after levenshtein
    :config
    (setq etab-bookmarks-file user-bookmarks-file)))

(defun rogue/init-focus ()
  (use-package focus))

(defun rogue/init-gscholar-bibtex ()
  (use-package gscholar-bibtex
    :config
    (setq gscholar-bibtex-database-file user-bib-file
          gscholar-bibtex-default-source "Google Scholar")))

(defun rogue/init-hackernews ()
  (use-package hackernews
    :bind ("C-c h" . hackernews)))

(defun rogue/init-hy-mode ()
  (use-package hy-mode
    :mode "\\.hy\\'"))

(defun rogue/init-kindle ()
  (use-package kindle
    :config
    (setq kindle-clipping-save-file user-clippings-file)))

(defun rogue/init-levenshtein ()
  (use-package levenshtein))

(defun rogue/init-mpm ()
  (use-package mpm
    :demand t
    :after org-cliplink))

(defun rogue/init-multiple-cursors ()
  (use-package multiple-cursors
    :bind (("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)
           ("C-M-<mouse-1>" . mc/add-cursor-on-click))))

(defun rogue/init-nov ()
  (use-package nov
    :config
    (push '("\\.epub\\'" . nov-mode) auto-mode-alist)
    (setq nov-text-width 80)))

(defun rogue/init-ob-async ()
  (use-package ob-async))

(defun rogue/init-ob-sagemath ()
  (use-package ob-sagemath))

(defun rogue/init-openwith ()
  (use-package openwith
    :demand t
    :config
    (setq openwith-associations
          (list
           (list (openwith-make-extension-regexp
                  '("mpg" "mpeg" "mp3" "mp4"
                    "avi" "wmv" "wav" "mov" "flv"
                    "ogm" "ogg" "mkv"))
                 "vlc"
                 '(file))
           (list (openwith-make-extension-regexp
                  '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
                 "libreoffice"
                 '(file))
           (list (openwith-make-extension-regexp
                  '("pdf" "ps" "ps.gz" "dvi"))
                 "okular"
                 '(file))))
    (openwith-mode t)))

(defun rogue/init-org-bbq ()
  (use-package org-bbq
    :after org))

(defun rogue/init-org-books ()
  (use-package org-books
    :config
    (setq org-books-file user-books-file)))

(defun rogue/init-org-cliplink ()
  (use-package org-cliplink
    :bind (("C-c y" . org-cliplink))))

(defun rogue/init-org-expand ()
  (use-package org-expand
    :bind (("C-c x" . helm-org-expand))))

(defun rogue/init-org-gcal ()
  (use-package org-gcal
    :ensure t
    :after (org calfw calfw-org)
    :hook ((cfw:calendar-mode . org-gcal-sync))
    :config
    (setq org-gcal-file-alist `(("abhinav.tushar.vs@gmail.com" . ,user-gcal-file)))
    ;; Secret file
    ;; (setq org-gcal-client-id "<>"
    ;;       org-gcal-client-secret "<>")
    (load-file (concat user-secrets-dir "gcal.el"))))

(defun rogue/init-org-gh ()
  (use-package org-gh
    :after org))

(defun rogue/init-org-journal ()
  (use-package org-journal
    :config
    (setq org-journal-dir user-journal-dir)
    (setq org-journal-enable-encryption t)))

(defun rogue/init-org-pretty-table ()
  (use-package org-pretty-table
    :demand t
    :init
    (add-hook 'org-mode-hook (lambda () (org-pretty-table-mode 1)))))

(defun rogue/init-org-make ()
  (use-package org-make
    :after org))

(defun rogue/init-parinfer ()
  (use-package parinfer
    :ensure t
    :bind (("C-," . parinfer-toggle-mode))
    :hook ((clojure-mode . parinfer-mode)
           (emacs-lisp-mode . parinfer-mode)
           (common-lisp-mode . parinfer-mode)
           (racket-mode . parinfer-mode)
           (lisp-mode . parinfer-mode)
           (scheme-mode . parinfer-mode)
           (hy-mode . parinfer-mode))
    :init
    (setq parinfer-extensions '(defaults
                                pretty-parens
                                smart-tab
                                smart-yank))))

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
    :after (s spotify levenshtein)))

(defun rogue/init-rogue-ligatures ()
  (use-package rogue-ligatures
    :after rogue-utils
    :config
    (rogue-ligatures-setup-general)
    (rogue-ligatures-setup-ess)))

(defun rogue/init-rogue-mu4e ()
  (use-package rogue-mu4e
    :after (authinfo mu4e openwith)
    :config
    (rogue-mu4e-setup)))

(defun rogue/init-rogue-org ()
  (use-package rogue-org
    :after org
    :config
    (rogue-org-setup-general)
    (rogue-org-setup-notes)
    (rogue-org-setup-babel)
    (rogue-org-setup-tex)))

(defun rogue/init-pile ()
  (use-package pile
    :after w
    :config
    (let ((preamble-template "<header>
                                <div class='site-title'>
                                  <a href='/'>
                                    <img src='/assets/images/avatar32.png'>
                                  </a>
                                </div>
                                <div class='site-nav'>
                                  <a href='/blog'> blog</a>
                                  <a href='/journal'> journal</a>
                                  <a href='/wiki'> wiki</a>
                                  <a href='/about'> about</a>
                                </div>
                                <div class='clearfix'></div>
                              </header>

                              <div class='page-header'>
                                <div class='page-meta'>%s</div>
                                <h1>%%t</h1>
                              </div>")
          (postamble "<footer id='footer'></footer>"))
      (setq pile-projects
            (list (pile-project :name "wiki"
                                :base-url "pile/wiki"
                                :input-dir (concat user-project-dir "pile/wiki")
                                :output-dir (concat user-project-dir "pile/docs/wiki")
                                :type 'wiki
                                :postamble postamble
                                :preamble (format preamble-template "Last modified: %d %C"))
                  (pile-project :name "blog"
                                :base-url "pile/blog"
                                :input-dir (concat user-project-dir "pile/blog")
                                :output-dir (concat user-project-dir "pile/docs/blog")
                                :type 'blog
                                :postamble postamble
                                :preamble (format preamble-template "%d"))
                  (pile-project :name "journal"
                                :base-url "pile/journal"
                                :input-dir (concat user-project-dir "pile/journal")
                                :output-dir (concat user-project-dir "pile/docs/journal")
                                :type 'blog
                                :postamble postamble
                                :preamble (format preamble-template "%d"))
                  (pile-project :name "assets"
                                :input-dir (concat user-project-dir "pile/assets")
                                :output-dir (concat user-project-dir "pile/docs/assets")
                                :type 'static)))
      (pile-setup))))

(defun rogue/init-rogue-processes ()
  (use-package rogue-processes
    :after rogue-utils
    :config
    (rogue-processes-define "offlineimap" "-o")
    (rogue-processes-define "mpm-play")
    ;; (rogue-processes-start-service "mpm-play")
    (setq rogue-processes-git-update-dirs nil)
    (rogue-processes-run-git-autoupdate-loop "10 min" 3600)))

(defun rogue/init-rogue-ui ()
  (use-package rogue-ui
    :after rogue-utils
    :config
    (rogue-ui-setup)))

(defun rogue/init-rogue-utils ()
  (use-package rogue-utils))

(defun rogue/init-sage-shell-mode ()
  (use-package sage-shell-mode
    :config
    (sage-shell:define-alias)
    (setq sage-shell-view-default-resolution 150)
    (add-hook 'sage-shell-mode-hook #'eldoc-mode)
    (add-hook 'sage-shell:sage-mode-hook #'eldoc-mode)
    (add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)))

(defun rogue/init-shell-switcher ()
  (use-package shell-switcher
    :demand t
    :config (setq shell-switcher-mode t)
    :bind (("C-'" . shell-switcher-switch-buffer)
           ("C-\"" . shell-switcher-new-shell))))

(defun rogue/init-snakemake-mode ()
  (use-package snakemake-mode
    :defer t))

(defun rogue/init-solaire-mode ()
  (use-package solaire-mode
    :hook ((prog-mode . turn-on-solaire-mode)
           (minibuffer-setup . solaire-mode-in-minibuffer)
           (ediff-prepare-buffer . solaire-mode))))

(defun rogue/init-spaceline-all-the-icons ()
  (progn
    (use-package spaceline-all-the-icons
      :after spaceline)
    (use-package spaceline
      :after powerline
      :config
      (setq-default mode-line-format '("%e" (:eval (spaceline-ml-ati)))))))

(defun rogue/init-swiper ()
  (use-package swiper
    :bind (("C-s" . swiper)
           ("C-r" . swiper))))

(defun rogue/init-switch-window ()
  (use-package switch-window
    :config
    (global-set-key (kbd "C-x o") 'switch-window)
    (setq switch-window-shortcut-style 'qwerty
          switch-window-qwerty-shortcuts '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o")
          switch-window-minibuffer-shortcut ?z)))

(defun rogue/init-viz ()
  (use-package viz
    :defer t))

(defun rogue/init-vue-mode ()
  (use-package vue-mode
    :mode ("\\.vue\\'" . vue-mode)))

(defun rogue/init-w ()
  (use-package w))

(defun rogue/init-weather-amherst ()
  (use-package weather-amherst
    :bind (("C-c w" . weather-amherst))))

(defun rogue/init-writegood-mode ()
  (use-package writegood-mode
    :defer t))
