;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(bibtex
     c-c++
     (clojure :variables
              clojure-enable-fancify-symbols t)
     common-lisp
     csv
     erlang
     emacs-lisp
     ess
     graphviz
     haskell
     html
     javascript
     (latex :variables
            latex-enable-folding t
            latex-enable-auto-fill t)
     (markdown :variables markdown-live-preview-engine 'vmd)
     (org :variables
          org-enable-github-support t
          org-enable-reveal-js-support t
          org-enable-org-journal-support t)
     (python :variables python-sort-imports-on-save t)
     racket
     ruby
     (rust :variables rust-format-on-save t)
     scheme
     shell
     shell-scripts
     sql
     typescript
     yaml
     ;; Assists
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-private-snippets-directory "~/.emacs.d/private/rogue/snippets")
     better-defaults
     colors
     (dash :variables helm-dash-docset-newpath "~/.local/share/Zeal/Zeal/docsets")
     ;; (elfeed :variables
     ;;         rmh-elfeed-org-files '("~/.emacs.d/private/rogue/feeds.org")
     ;;         elfeed-enable-web-interface t)
     git
     github
     (go :variables go-tab-width 4)
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     imenu-list
     (mu4e :variables mu4e-installation-path "/usr/share/emacs/site-lisp/mu4e")
     pandoc
     pdf
     prodigy
     (ranger :variables ranger-show-preview t)
     restclient
     rogue
     (spell-checking :variables spell-checking-enable-by-default nil)
     spotify
     (syntax-checking :variables syntax-checking-enable-by-default nil)
     theming
     typography
     (version-control :variables
                      version-control-diff-tool 'git-gutter
                      version-control-global-margin t
                      version-control-diff-side 'left)
     xkcd)
   dotspacemacs-additional-packages '()
   dotspacemacs-excluded-packages '(vi-tilde-fringe ess-R-object-popup)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style '(hybrid :variables
                                       hybrid-mode-default-state 'hybrid)
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'org-mode
   dotspacemacs-themes '(doom-molokai spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state nil
   dotspacemacs-default-font '("Iosevka"
                               :size 17
                               :weight regular
                               :width normal
                               :powerline-scale 1.0)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts t
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header t
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers '(:relative nil :disabled-for-modes text-mode)
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  ;; Directories
  (defconst user-layer-dir (file-name-as-directory "~/.emacs.d/private/rogue"))
  (defconst user-secrets-dir (file-name-as-directory (concat user-layer-dir "secrets")))
  (defconst user-cloud-dir (file-name-as-directory (getenv "CLOUD_DIR")))
  (defconst user-project-dir (file-name-as-directory (getenv "PROJECTS_DIR")))

  ;; Derived directories
  (defconst user-notes-dir (file-name-as-directory (concat user-cloud-dir "Notes/personal")))
  (defconst user-journal-dir (file-name-as-directory (concat user-notes-dir "journal")))
  (defconst user-pdfs-dir (file-name-as-directory (concat user-notes-dir "pdfs")))

  ;; Files
  (defconst user-bib-file (concat user-notes-dir "library.bib"))
  (defconst user-bib-notes-file (concat user-project-dir "lepisma.github.io/wiki/readings/bib-notes.org"))
  (defconst user-books-file (concat user-project-dir "lepisma.github.io/wiki/readings/books.org"))
  (defconst user-clippings-file (concat user-project-dir "lepisma.github.io/wiki/readings/clippings.org"))

  ;; Separate custom stuff
  (setq custom-file "~/.emacs-custom.el")
  (load custom-file))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place you code here."

  (setq user-full-name "Abhinav Tushar"
        browse-url-generic-program (executable-find (getenv "BROWSER"))
        browse-url-browser-function 'browse-url-generic)

  (server-start)

  ;; Language specific settings
  (setq-default web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2
                css-indent-offset 2
                js2-basic-offset 2
                js-indent-level 2
                js2-strict-missing-semi-warning nil
                js2-missing-semi-one-line-override nil
                typescript-indent-level 2)

  ;; Load PG if found locally
  (when (f-exists? "~/.emacs.d/PG")
    (load "~/.emacs.d/PG/generic/proof-site")
    (add-hook 'coq-mode-hook #'solaire-mode))

  (setf slime-lisp-implementations
        `((sbcl    ("sbcl"))
          (roswell ("ros" "-Q" "run"))
          (roswell-dune ("ros" "-Q" "run" "-e" "(ql:quickload :dune)" "-e" "(in-package :dune)")))
        slime-default-lisp 'roswell-dune)

  (slime-setup '(slime-asdf
                 slime-company
                 slime-fancy
                 slime-indentation
                 slime-sbcl-exts
                 slime-scratch
                 slime-tramp)))
