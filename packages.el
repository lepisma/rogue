;;; packages.el --- rogue Layer packages File for Spacemacs

(defvar rogue-packages nil)

(defmacro r|pkg (name &rest body)
  (declare (indent defun))
  (let ((id (if (listp name) (car name) name)))
    `(progn
       (defun ,(intern (format "rogue/init-%s" id)) ()
         (use-package ,id ,@body))
       (push ',name rogue-packages))))

(r|pkg (bmp :location (recipe :fetcher github :repo "lepisma/bmp")))

(r|pkg (conceal :location (recipe :fetcher github :repo "lepisma/conceal"))
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (when (conceal-buffer-gpg-p (current-buffer))
                (conceal-mode 1)))))

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
  :after (all-the-icons treemacs r-ui)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-treemacs-line-spacing 4
        doom-treemacs-enable-variable-pitch t)

  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(r|pkg (elml :location (recipe :fetcher github :repo "lepisma/elml")))

(r|pkg emojify)

(r|pkg enlive)

(r|pkg eros
  :config
  (setq eros-eval-result-prefix "▶ ")
  (eros-mode 1))

(r|pkg (esi :location (recipe :fetcher github :repo "lepisma/emacs-speech-input"))
  :after (whisper)
  :bind (("C-c t" . esi-transcribe)))

(r|pkg goto-line-preview
  :bind ("M-g g" . goto-line-preview-goto-line))

(r|pkg keepass-mode)

(r|pkg (kindle :location local)
  :config
  (setq kindle-clipping-save-file user-clippings-file))

(r|pkg move-text
  :config
  (move-text-default-bindings))

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
           "onlyoffice-desktopeditors"
           (file))
          (,(openwith-make-extension-regexp
             '("pdf" "ps" "ps.gz" "dvi"))
           "okular"
           (file)))))

(r|pkg (org-books :location (recipe :fetcher github :repo "lepisma/org-books"))
  :config
  (setq org-books-file user-books-file))

(r|pkg org-fragtog
  :hook ((org-mode . org-fragtog-mode)))

(r|pkg org-roam
  :ensure t
  :custom
  (org-roam-directory user-notes-dir)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
        org-roam-capture-templates `(("n" "default" plain "%?"
                                      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                         "#+TITLE: ${title}\n\n")
                                      :unnarrowed)
                                     ("l" "literature" plain "%?"
                                      :target (file+head "literature/%<%Y%m%d%H%M%S>-${slug}.org"
                                                         "#+TITLE: ${title}\n\n")
                                      :unnarrowed))
        org-roam-capture-ref-templates `(("l" "literature" plain "%?"
                                          :target (file+head "literature/%<%Y%m%d%H%M%S>-${slug}.org"
                                                             "#+TITLE: ${title}\n\n")
                                          :unnarrowed t)))
  (require 'org-roam-protocol)
  (org-roam-db-autosync-mode))

(r|pkg org-roam-ui
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(r|pkg org-super-agenda
  :after org
  :config
  (org-super-agenda-mode))

(r|pkg org-web-tools
  :after org)

(r|pkg org-variable-pitch
  :hook
  ((after-init . org-variable-pitch-setup)))

(r|pkg ov)

(r|pkg (pile :location (recipe :fetcher github :repo "lepisma/pile"))
  :after (f ht mustache r-utils w)
  :config
  (let* ((template-dir (concat user-layer-dir "misc/"))
         (preamble-template (f-read-text (concat template-dir "pile-preamble.html.template") 'utf-8))
         (postamble-template (f-read-text (concat template-dir "pile-postamble.html.template") 'utf-8))
         (root-url "https://lepisma.xyz/")
         (input-dir (concat user-cloud-dir "lepisma.github.io/"))
         (output-dir (concat user-project-dir "lepisma.github.io-deploy/")))
    (setq pile-serve-dir output-dir
          pile-projects
          (list (pile-project-wiki :name "wiki"
                                   :root-url root-url
                                   :base-url "wiki"
                                   :input-dir (concat input-dir "wiki")
                                   :output-dir (concat output-dir "wiki")
                                   :postamble postamble-template
                                   :preamble (mustache-render preamble-template (ht ("wiki-p" t) ("page-meta" "Last modified: %d %C")))
                                   :setupfile (concat input-dir "assets/export.setup"))
                (pile-project-blog :name "blog"
                                   :root-url root-url
                                   :base-url ""
                                   :input-dir (concat input-dir "blog")
                                   :output-dir output-dir
                                   :postamble postamble-template
                                   :preamble (mustache-render preamble-template (ht ("blog-p" t) ("page-meta" "%d")))
                                   :setupfile (concat input-dir "assets/export.setup"))
                (pile-project-blog :name "journal"
                                   :root-url root-url
                                   :base-url "journal"
                                   :input-dir (concat input-dir "journal")
                                   :output-dir (concat output-dir "journal")
                                   :postamble postamble-template
                                   :preamble (mustache-render preamble-template (ht ("journal-p" t) ("page-meta" "%d")))
                                   :setupfile (concat input-dir "assets/export.setup"))
                (pile-project-blog :name "log"
                                   :root-url root-url
                                   :base-url "log"
                                   :input-dir (concat input-dir "log")
                                   :output-dir (concat output-dir "log")
                                   :postamble postamble-template
                                   :preamble (mustache-render preamble-template (ht ("log-p" t) ("page-meta" "%d"))))
                (pile-project-static :name "assets"
                                     :root-url root-url
                                     :input-dir (concat input-dir "assets")
                                     :output-dir (concat output-dir "assets"))
                (pile-project-plain :name "misc"
                                    :root-url root-url
                                    :base-url ""
                                    :input-dir (concat input-dir "misc")
                                    :output-dir output-dir)))
    ;; Setup notes here to get the wiki files in agenda
    (r-org/setup-notes)
    (pile-setup)
    (r-utils/add-hooks '(pile-pre-publish-hook)
                       (list #'pile-hooks-pre-add-setupfile
                             #'pile-hooks-pre-add-bc
                             #'pile-hooks-pre-add-cids
                             #'pile-hooks-pre-add-date
                             #'pile-hooks-pre-add-dropcap
                             #'pile-hooks-pre-add-tags
                             #'pile-hooks-pre-add-crosslinks
                             #'pile-hooks-pre-add-draft-watermark))
    (r-utils/add-hooks '(pile-post-publish-hook)
                       (list #'pile-hooks-post-clear-cids
                             #'pile-hooks-post-generate-atom
                             #'pile-hooks-post-generate-archive
                             #'pile-hooks-post-stringify-title
                             #'pile-hooks-post-sync-static-files
                             #'pile-hooks-post-generate-index))))

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

(r|pkg ligature
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

 (global-ligature-mode 't))

(r|pkg (r-mu4e :location local)
  :after (mu4e openwith message mml cl-macs s)
  :bind (:map mu4e-compose-mode-map
              (("C-c C-c" . r-mu4e/send))
              :map mu4e-view-mode-map
              (("C-c d" . epa-mail-decrypt))
              :map mu4e-main-mode-map
              (("u" . mu4e-update-index)))
  :config
  (r-mu4e/setup))

(r|pkg (r-org :location local)
  :after (org pile hydra)
  :config
  (r-org/setup-general)
  ;; Notes setup is done after pile
  ;; (r-org-setup-notes)
  (r-org/setup-babel)
  (r-org/setup-tex))

(r|pkg (r-ui :location local)
  :after (all-the-icons treemacs r-utils doom-modeline)
  :config
  (r-ui/setup))

(r|pkg (r-utils :location local))

(r|pkg shell-switcher
  :config (setq shell-switcher-mode t)
  :bind (("C-'" . shell-switcher-switch-buffer)
         ("C-\"" . shell-switcher-new-shell)))

(r|pkg solaire-mode
  :hook ((prog-mode . solaire-mode)
         (ediff-prepare-buffer . solaire-mode)))

(r|pkg svg-tag-mode
  :config
  (setq svg-tag-tags
        '((":TODO:" . ((lambda (tag) (svg-tag-make tag :beg 1 :end -1)))))))

(r|pkg swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(r|pkg switch-window
  :config
  (global-set-key (kbd "C-x o") 'switch-window)
  (setq switch-window-shortcut-style 'qwerty
        switch-window-qwerty-shortcuts '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o")
        switch-window-minibuffer-shortcut ?z))

(r|pkg unicode-fonts
  :config
  (unicode-fonts-setup))

(r|pkg (w :location (recipe :fetcher github :repo "lepisma/w.el")))

(r|pkg web-server
  :after htmlize)
