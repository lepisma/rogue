;;; packages.el --- rogue Layer packages File for Spacemacs

(defvar rogue-packages nil)

(defmacro r|pkg (name &rest body)
  (declare (indent defun))
  (let ((id (if (listp name) (car name) name)))
    `(progn
       (defun ,(intern (format "rogue/init-%s" id)) ()
         (use-package ,id ,@body))
       (push ',name rogue-packages))))

(r|pkg all-the-icons)

(r|pkg beacon
  :config
  (beacon-mode)
  (setq beacon-color (face-attribute 'region :background nil t)
        beacon-blink-when-buffer-changes t
        beacon-blink-when-point-moves-vertically nil))

(r|pkg (bmp :location (recipe :fetcher github :repo "lepisma/bmp")))

(r|pkg buttercup)

(r|pkg (calibre :location (recipe :fetcher github :repo "lepisma/calibre.el"))
  :after (s dash-functional)
  :config
  (setq calibre-root (concat user-cloud-dir "Calibre Shared")))

(r|pkg chronos
  :config
  ;; Chronos looks like abandoned so I am putting all the changes/fixes here
  ;; instead of possibly doing a PR.
  (defun chronos-buffer-switch (_)
    (switch-to-buffer chronos-buffer-name))

  (defun chronos--format-notification (n)
    (concat "" (cadr n)))

  (defun chronos--display-clock ()
    (insert (propertize (format "⌛%s" (chronos--time-string-rounded-to-minute (current-time)))
                        'face 'chronos-notification-clock)))

  (setq chronos-shell-notify-program "mplayer"
        chronos-shell-notify-parameters '("/usr/share/sounds/freedesktop/stereo/complete.oga")
        chronos-expiry-functions '(chronos-dunstify chronos-shell-notify chronos-buffer-notify chronos-buffer-switch)))

(r|pkg colormaps)

(r|pkg company-box
  :after company
  :config
  (add-hook 'text-mode-hook #'company-box-mode))

(r|pkg conda
  :config
  (conda-env-initialize-eshell)
  (setq conda-anaconda-home (expand-file-name "~/.miniconda")))

(r|pkg cricbuzz)

(r|pkg (dg :location local)
  :after (elml web-server))

(r|pkg dired-subtree
  :after ranger
  :bind (:map ranger-mode-map
              (("<tab>" . dired-subtree-toggle)
               ("<backtab>" . dired-subtree-cycle))))

(r|pkg direnv
  :config
  (direnv-mode))

(r|pkg dockerfile-mode)

(r|pkg doom-themes
  :after (treemacs r-ui)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-treemacs-line-spacing 4
        doom-treemacs-enable-variable-pitch t)

  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(r|pkg (elml :location (recipe :fetcher github :repo "lepisma/elml")))

(r|pkg enlive)

(r|pkg eros
  :config
  (setq eros-eval-result-prefix "▶ ")
  (eros-mode 1))

(r|pkg goto-line-preview
  :bind ("M-g g" . goto-line-preview-goto-line))

(r|pkg gscholar-bibtex
  :config
  (setq gscholar-bibtex-database-file user-bib-file
        gscholar-bibtex-default-source "Google Scholar"))

(r|pkg helm-chronos
  :after chronos
  :bind (("C-c t" . helm-chronos-add-timer))
  :config
  ;; Fix from https://github.com/dxknight/helm-chronos/pull/2
  (setq helm-chronos--fallback-source
    (helm-build-dummy-source "Enter <expiry time spec>/<message>"
      :filtered-candidate-transformer
      (lambda (_candidates _source)
        (list (or (and (not (string= helm-pattern ""))
                       helm-pattern)
                  "Enter a timer to start")))
      :action '(("Add timer" . (lambda (candidate)
                                 (if (string= helm-pattern "")
                                     (message "No timer")
                                   (helm-chronos--parse-string-and-add-timer helm-pattern)))))))
  (setq helm-chronos-standard-timers '()))

(r|pkg helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

(r|pkg htmlize)

(r|pkg hy-mode
  :mode "\\.hy\\'")

(r|pkg (iorg :location (recipe :fetcher github :repo "lepisma/iorg"))
  :hook ((org-mode . iorg-mode)))

(r|pkg (kindle :location local)
  :config
  (setq kindle-clipping-save-file user-clippings-file))

(r|pkg (levenshtein :location (recipe :fetcher github :repo "emacsorphanage/levenshtein")))

(r|pkg minimap
  :demand t
  :config
  (setq minimap-highlight-line nil
        minimap-window-location 'right))

(r|pkg (mu4e-fold :location local)
  :after r-mu4e
  :bind (:map mu4e-headers-mode-map ("<tab>" . mu4e-headers-toggle-thread-folding))
  :hook ((mu4e-headers-found . mu4e-headers-fold-all)))

(r|pkg multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M-<mouse-1>" . mc/add-cursor-on-click)))

(r|pkg mustache)

(r|pkg ob-async)

(r|pkg ob-sagemath)

(r|pkg (obtt :location (recipe :fetcher github :repo "lepisma/obtt"))
  :after org
  :config (setq obtt-templates-dir (concat user-layer-dir "obtt/")))

(r|pkg openwith
  :config
  (setq openwith-associations
        `((,(openwith-make-extension-regexp
             '("mpg" "mpeg" "mp3" "mp4"
               "avi" "wmv" "wav" "mov" "flv"
               "ogm" "ogg" "mkv"))
           "vlc"
           (file))
          (,(openwith-make-extension-regexp
             '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
           "libreoffice"
           (file))
          (,(openwith-make-extension-regexp
             '("pdf" "ps" "ps.gz" "dvi"))
           "okular"
           (file)))))

(r|pkg (org-bbq :location
                (recipe :fetcher url
                        :url "https://raw.githubusercontent.com/lepisma/bbq/master/emacs/org-bbq.el"))
  :after org)

(r|pkg (org-books :location (recipe :fetcher github :repo "lepisma/org-books"))
  :config
  (setq org-books-file user-books-file))

(r|pkg org-cliplink
  :bind (("C-c y" . org-cliplink)))

(r|pkg org-edna)

(r|pkg org-journal
  :custom
  (org-journal-dir user-journal-dir)
  (org-journal-enable-encryption t))

(r|pkg org-kanban)

(r|pkg (org-pretty-table :location (recipe :fetcher github :repo "Fuco1/org-pretty-table")))
  ;; :config
  ;; (add-hook 'org-mode-hook #'org-pretty-table-mode))

(r|pkg (org-make :location local)
  :after org)

(r|pkg org-super-agenda
  :after org
  :config
  (org-super-agenda-mode))

(r|pkg org-web-tools
  :after org)

(r|pkg org-variable-pitch
  :config
  (setq org-variable-pitch-fixed-font "Iosevka"
        org-variable-pitch-fixed-faces '(org-block
                                         org-block-begin-line
                                         org-block-end-line
                                         org-code
                                         org-document-info-keyword
                                         org-done
                                         org-formula
                                         org-indent
                                         org-meta-line
                                         org-property-value
                                         org-special-keyword
                                         org-table
                                         org-todo
                                         org-verbatim
                                         org-date))
  :hook
  ((org-mode . org-variable-pitch-minor-mode)))

(r|pkg (outline-wiki :location (recipe :fetcher github :repo "lepisma/outline-wiki.el")))

(r|pkg ov)

(r|pkg parinfer
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
                               smart-yank)))

(r|pkg (pile :location (recipe :fetcher github :repo "lepisma/pile"))
  :after (f ht mustache r-utils w)
  :config
  (let* ((template-dir (concat user-layer-dir "misc/"))
         (preamble-template (f-read-text (concat template-dir "pile-preamble.html.template") 'utf-8))
         (postamble-template (f-read-text (concat template-dir "pile-postamble.html.template") 'utf-8))
         (root-url "https://lepisma.xyz/")
         (output-dir (concat user-project-dir "lepisma.github.io-deploy/")))
    (setq pile-serve-dir output-dir
          pile-projects
          (list (pile-project-wiki :name "wiki"
                                   :root-url root-url
                                   :base-url "wiki"
                                   :input-dir (concat user-project-dir "lepisma.github.io/wiki")
                                   :output-dir (concat output-dir "wiki")
                                   :postamble postamble-template
                                   :preamble (mustache-render preamble-template (ht ("wiki-p" t) ("page-meta" "Last modified: %d %C"))))
                (pile-project-blog :name "blog"
                                   :root-url root-url
                                   :base-url ""
                                   :input-dir (concat user-project-dir "lepisma.github.io/blog")
                                   :output-dir output-dir
                                   :postamble postamble-template
                                   :preamble (mustache-render preamble-template (ht ("blog-p" t) ("page-meta" "%d"))))
                (pile-project-blog :name "journal"
                                   :root-url root-url
                                   :base-url "journal"
                                   :input-dir (concat user-project-dir "lepisma.github.io/journal")
                                   :output-dir (concat output-dir "journal")
                                   :postamble postamble-template
                                   :preamble (mustache-render preamble-template (ht ("journal-p" t) ("page-meta" "%d"))))
                (pile-project-blog :name "log"
                                   :root-url root-url
                                   :base-url "log"
                                   :input-dir (concat user-project-dir "lepisma.github.io/log")
                                   :output-dir (concat output-dir "log")
                                   :postamble postamble-template
                                   :preamble (mustache-render preamble-template (ht ("log-p" t) ("page-meta" "%d"))))
                (pile-project-static :name "assets"
                                     :root-url root-url
                                     :input-dir (concat user-project-dir "lepisma.github.io/assets")
                                     :output-dir (concat output-dir "assets"))
                (pile-project-plain :name "misc"
                                    :root-url root-url
                                    :base-url ""
                                    :input-dir (concat user-project-dir "lepisma.github.io/misc")
                                    :output-dir output-dir
                                    :postamble ""
                                    :preamble "")))
    ;; Setup notes here to get the wiki files in agenda
    (r-org/setup-notes)
    (pile-setup)
    (r-utils/add-hooks '(pile-pre-publish-hook)
                       (list #'pile-hooks-pre-add-bc
                             #'pile-hooks-pre-add-cids
                             #'pile-hooks-pre-add-date
                             #'pile-hooks-pre-add-dropcap
                             #'pile-hooks-pre-add-tags
                             #'pile-hooks-pre-add-crosslinks))
    (r-utils/add-hooks '(pile-post-publish-hook)
                       (list #'pile-hooks-post-clear-cids
                             #'pile-hooks-post-generate-atom
                             #'pile-hooks-post-generate-archive
                             #'pile-hooks-post-stringify-title
                             #'pile-hooks-post-generate-index))))

(r|pkg powerthesaurus)

(r|pkg pretty-mode
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
   '(:greek :arithmetic-nary)))

(r|pkg protobuf-mode
  :mode "\\.proto\\'")

(r|pkg (r-feeds :location local)
  :after (elfeed helm)
  :bind (("C-c f" . helm-elfeed))
  :config
  (setq r-feeds-filters '(("Default" . "@6-months-ago +unread -freq -podcast")
                          ("All" . "@6-months-ago +unread")
                          ("Frequent" . "@6-months-ago +unread +freq")
                          ("Media" . "@6-months-ago +unread +media"))
        r-feeds-dump-file (concat user-notes-dir "personal/" "elfeed-dump.org"))
  (add-hook 'elfeed-db-update-hook (lambda () (r-feeds/elfeed-to-org r-feeds-dump-file)))
  (setq-default elfeed-search-filter (alist-get "Default" r-feeds-filters nil nil #'string-equal)))

(r|pkg (r-kv :location local)
  :config
  (setq r-kv-file (concat user-layer-dir "misc/" "rkv.el")))

(r|pkg (read-lyrics :location (recipe :fetcher github :repo "lepisma/read-lyrics.el"))
  :after (s levenshtein)
  :config
  (defun read-lyrics-get-bbq ()
    "Return artist, track pair from bbq."
    (let ((res (->> (shell-command-to-string "bbq :state")
                  (json-read-from-string)
                  (assoc 'item)
                  (cdr))))
      (when (consp res)
        (cons (alist-get 'artist res)
              (alist-get 'title res)))))

  (add-to-list 'read-lyrics-getters #'read-lyrics-get-bbq))

(r|pkg realgud
  :config
  (setq realgud:pdb-command-name "python -m pdb"))

(r|pkg (r-ligatures :location local)
  :after r-utils
  :config
  (r-ligatures/setup-general)
  (r-ligatures/setup-ess))

(r|pkg (r-mu4e :location local)
  :after (mu4e openwith message mml cl-macs s)
  :bind (:map mu4e-compose-mode-map
              (("C-c C-c" . r-mu4e/sign-and-send)
               ("C-c e" . mml-secure-encrypt-pgp))
              :map mu4e-view-mode-map
              (("C-c d" . epa-mail-decrypt))
              :map mu4e-main-mode-map
              (("u" . mu4e-update-index)))
  :config
  (r-mu4e/setup))

(r|pkg (r-org :location local)
  :after (org pile org-edna)
  :config
  (r-org/setup-general)
  ;; Notes setup is done after pile
  ;; (r-org-setup-notes)
  (r-org/setup-babel)
  (r-org/setup-tex))

(r|pkg (r-ui :location local)
  :after (treemacs r-utils)
  :config
  (r-ui/setup))

(r|pkg (r-utils :location local))

(r|pkg sage-shell-mode
  :config
  (sage-shell:define-alias)
  (setq sage-shell-view-default-resolution 150)
  (add-hook 'sage-shell-mode-hook #'eldoc-mode)
  (add-hook 'sage-shell:sage-mode-hook #'eldoc-mode)
  (add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode))

(r|pkg shell-switcher
  :config (setq shell-switcher-mode t)
  :bind (("C-'" . shell-switcher-switch-buffer)
         ("C-\"" . shell-switcher-new-shell)))

(r|pkg snakemake-mode)

(r|pkg solaire-mode
  :hook ((prog-mode . turn-on-solaire-mode)
         (minibuffer-setup . solaire-mode-in-minibuffer)
         (ediff-prepare-buffer . solaire-mode)))

(r|pkg (spaceline-all-the-icons :location local)
  :after spaceline
  :config
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-ati)))))

(r|pkg sublimity
  :demand t
  :config
  (require 'sublimity-scroll)
  (setq sublimity-scroll-weight 5
        sublimity-scroll-drift-length 1)
  (sublimity-mode 1))

(r|pkg swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(r|pkg switch-window
  :config
  (global-set-key (kbd "C-x o") 'switch-window)
  (setq switch-window-shortcut-style 'qwerty
        switch-window-qwerty-shortcuts '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o")
        switch-window-minibuffer-shortcut ?z))

(r|pkg tj3-mode
  :mode ("\\.tjp\\'" . tj3-mode))

(r|pkg (viz :location local))

(r|pkg vue-mode
  :mode ("\\.vue\\'" . vue-mode))

(r|pkg unicode-fonts
  :config
  (unicode-fonts-setup))

(r|pkg (w :location (recipe :fetcher github :repo "lepisma/w.el")))

(r|pkg web-server
  :after htmlize)

(r|pkg writegood-mode)
