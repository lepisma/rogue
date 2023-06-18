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
     (common-lisp :variables
                  slime-lisp-implementations `((sbcl    ("sbcl"))
                                               (roswell ("ros" "-Q" "run")))
                  slime-default-lisp 'roswell)
     csv
     emacs-lisp
     (go :variables
         go-backend 'lsp
         go-format-before-save t
         go-tab-width 4)
     graphviz
     haskell
     (html :variables
           web-mode-markup-indent-offset 2
           web-mode-css-indent-offset 2
           web-mode-code-indent-offset 2
           css-indent-offset 2)
     (javascript :variables
                 js2-basic-offset 2
                 js-indent-level 2
                 js2-strict-missing-semi-warning nil
                 js2-missing-semi-one-line-override nil
                 typescript-indent-level 2)
     (latex :variables
            latex-enable-folding t
            latex-enable-auto-fill t)
     (lsp :variables
          lsp-message-project-root-warning t)
     (markdown :variables markdown-live-preview-engine 'vmd)
     (org :variables
          org-enable-appear-support t
          org-enable-reveal-js-support t
          org-enable-org-journal-support t
          org-enable-roam-support t
          org-enable-roam-protocol t
          org-enable-roam-ui t
          org-enable-verb-support t
          org-enable-modern-support t)
     (python :variables
             python-backend 'lsp
             python-lsp-server 'pylsp
             python-test-runner 'pytest
             python-sort-imports-on-save t
             python-fill-docstring-style 'django)
     (rust :variables
           rust-format-on-save t
           rust-backend 'lsp)
     (shell :variables
            shell-default-shell 'vterm)
     shell-scripts
     sql
     yaml
     ;; Assists
     (auto-completion :variables
                      company-quickhelp-delay 0.8
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-private-snippets-directory "~/.emacs.d/private/rogue/snippets")
     better-defaults
     colors
     dap
     (elfeed :variables
             rmh-elfeed-org-files '("~/.emacs.d/private/rogue/feeds.org")
             elfeed-enable-web-interface t
             elfeed-goodies/show-mode-padding 2
             elfeed-goodies/powerline-default-separator 'slant
             elfeed-goodies/feed-source-column-width 25
             elfeed-goodies/tag-column-width 30)
     epub
     git
     helpful
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     imenu-list
     (languagetool :variables
                   langtool-default-language "en-US"
                   langtool-show-error-on-jump t
                   langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")
     (mu4e :variables
           mu4e-installation-path "/usr/share/emacs/site-lisp/mu4e"
           mu4e-enable-async-operations t)
     pandoc
     parinfer
     pdf
     prodigy
     (ranger :variables ranger-show-preview t)
     restclient
     rogue
     spell-checking
     spotify
     syntax-checking
     systemd
     theming
     (treemacs :variables
               treemacs-follow-after-init nil
               treemacs-use-follow-mode t
               treemacs-use-all-the-icons-theme t)
     typography
     (version-control :variables
                      version-control-diff-tool 'git-gutter
                      version-control-global-margin t
                      version-control-diff-side 'left)
     xkcd)
   dotspacemacs-mode-line-theme 'doom
   dotspacemacs-additional-packages '()
   dotspacemacs-excluded-packages '(vi-tilde-fringe)
   dotspacemacs-delete-orphan-packages nil))

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
   dotspacemacs-editing-style 'emacs
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists nil
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   dotspacemacs-themes '(doom-molokai spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state nil
   dotspacemacs-default-font '("Iosevka"
                               :size 15
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

  (defconst user-data-dir (file-name-as-directory (getenv "DATA_DIR")))
  (defconst user-cloud-dir (file-name-as-directory (getenv "CLOUD_DIR")))
  (defconst user-project-dir (file-name-as-directory (getenv "PROJECTS_DIR")))

  ;; Task manager directory
  (defconst user-tasks-dir (file-name-as-directory (concat user-cloud-dir "tasks")))

  ;; Derived directories
  (defconst user-notes-dir (file-name-as-directory (concat user-cloud-dir "notes")))
  (defconst user-journal-dir (file-name-as-directory (concat user-notes-dir "journal")))
  (defconst user-pdfs-dir (file-name-as-directory (concat user-notes-dir "pdfs")))

  ;; Files
  (defconst user-bib-file (concat user-notes-dir "library.bib"))
  (defconst user-bib-notes-file (concat user-cloud-dir "lepisma.github.io/wiki/readings/notes/documents.org"))
  (defconst user-books-file (concat user-cloud-dir "lepisma.github.io/wiki/readings/reading-list.org"))
  (defconst user-clippings-file (concat user-cloud-dir "lepisma.github.io/wiki/readings/clippings.org"))

  (setq auth-sources '("~/.authinfo.gpg"))

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
        browse-url-generic-program (executable-find "www")
        browse-url-browser-function 'browse-url-generic)

  ;; Mac specific settings
  (setq mac-command-modifier 'control)

  (delete-selection-mode t)
  (server-start)
  (setq evil-emacs-state-cursor 'bar)

  ;; A few late setup calls
  ;; TODO: There is some load order issue. Put these in the right place
  (openwith-mode)
  (require 'r-mu4e)
  (require 'r-ui)

  (slime-setup '(slime-asdf
                 slime-company
                 slime-fancy
                 slime-indentation
                 slime-sbcl-exts
                 slime-scratch
                 slime-tramp)))
