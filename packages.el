;;; packages.el --- rogue Layer packages File for Spacemacs

(defvar rogue-packages nil)

(defmacro rpkg (name &rest body)
  (declare (indent defun))
  (let ((id (if (listp name) (car name) name)))
    `(progn
       (defun ,(intern (format "rogue/init-%s" id)) ()
         (use-package ,id ,@body))
       (push ',name rogue-packages))))

(rpkg all-the-icons)

(rpkg beacon
  :config
  (beacon-mode)
  (setq beacon-color (face-attribute 'region :background nil t)
        beacon-blink-when-buffer-changes t
        beacon-blink-when-point-moves-vertically nil))

(rpkg (bmp :location (recipe :fetcher github :repo "lepisma/bmp")))

(rpkg browse-at-remote)

(rpkg cricbuzz)

(rpkg (calibre :location (recipe :fetcher github :repo "lepisma/calibre.el"))
  :after (s dash-functional)
  :config
  (setq calibre-root (concat user-cloud-dir "Calibre Shared")))

(rpkg colormaps)

(rpkg company-box
  :after company
  :config
  (add-hook 'text-mode-hook #'company-box-mode))

(rpkg conda
  :config
  (conda-env-initialize-eshell)
  (setq conda-anaconda-home (expand-file-name "~/.miniconda")))

(rpkg dired-subtree
  :after ranger
  :bind (:map ranger-mode-map
              (("<tab>" . dired-subtree-toggle)
               ("<backtab>" . dired-subtree-cycle))))

(rpkg doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (doom-themes-neotree-config)
  (setq doom-neotree-enable-variable-pitch t
        doom-neotree-file-icons 'simple
        doom-neotree-line-spacing 4)
  (doom-themes-org-config))

(rpkg (duck :location (recipe :fetcher github :repo "lepisma/duck.el"))
  :config (setq duck-cli-path "~/.cache/duckling-cli-arch-x86-64"))

(rpkg (r-feeds :location local)
  :after (elfeed helm)
  :bind (("C-c f" . helm-elfeed))
  :config
  (setq r-feeds-filters '(("Default" . "@6-months-ago +unread -freq -podcast")
                          ("All" . "@6-months-ago +unread")
                          ("Frequent" . "@6-months-ago +unread +freq")
                          ("Media" . "@6-months-ago +unread +podcast +video")))
  (setq-default elfeed-search-filter (alist-get "Default" r-feeds-filters nil nil #'string-equal)))

(rpkg enlive)

(rpkg gscholar-bibtex
  :config
  (setq gscholar-bibtex-database-file user-bib-file
        gscholar-bibtex-default-source "Google Scholar"))

(rpkg helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

(rpkg hy-mode
  :mode "\\.hy\\'")

(rpkg (iorg :location (recipe :fetcher github :repo "lepisma/iorg"))
  :hook ((org-mode . iorg-mode)))

(rpkg (kindle :location local)
  :config
  (setq kindle-clipping-save-file user-clippings-file))

(rpkg (levenshtein :location (recipe :fetcher github :repo "emacsorphanage/levenshtein")))

(rpkg (mpm :location
           (recipe :fetcher url
                   :url "https://raw.githubusercontent.com/lepisma/mpm/master/emacs/mpm.el"))
  :demand t
  :after org-cliplink)

(rpkg (mu4e-fold :location local)
  :after r-mu4e
  :bind (:map mu4e-headers-mode-map ("<tab>" . mu4e-headers-toggle-thread-folding))
  :hook ((mu4e-headers-found . mu4e-headers-fold-all)))

(rpkg multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M-<mouse-1>" . mc/add-cursor-on-click)))

(rpkg ob-async)

(rpkg ob-sagemath)

(rpkg (obtt :location (recipe :fetcher github :repo "lepisma/obtt"))
  :after org
  :config (setq obtt-templates-dir (concat user-layer-dir "obtt/")))

(rpkg openwith
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
  (openwith-mode))

(rpkg (org-bbq :location
               (recipe :fetcher url
                       :url "https://raw.githubusercontent.com/lepisma/bbq/master/emacs/org-bbq.el"))
  :after org)

(rpkg (org-books :location (recipe :fetcher github :repo "lepisma/org-books"))
  :config
  (setq org-books-file user-books-file))

(rpkg org-cliplink
  :bind (("C-c y" . org-cliplink)))

(rpkg org-journal
  :config
  (setq org-journal-dir user-journal-dir)
  (setq org-journal-enable-encryption t))

(rpkg (org-pretty-table :location (recipe :fetcher github :repo "Fuco1/org-pretty-table"))
  :demand t
  :init
  (add-hook 'org-mode-hook #'org-pretty-table-mode))

(rpkg (org-make :location local)
  :after org)

(rpkg org-super-agenda
  :after org
  :config
  (org-super-agenda-mode))

(rpkg org-web-tools
  :after org)

(rpkg parinfer
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

(rpkg (pile :location (recipe :fetcher github :repo "lepisma/pile"))
  :after w
  :config
  (let ((preamble-template "<header>
                                <div class='site-title'>
                                  <a href='/'>
                                    <img src='/assets/images/avatar32.png'>
                                  </a>
                                </div>
                                <div class='site-nav'>
                                  <a class='%s' href='/'> blog</a>
                                  <a class='%s' href='/journal'> journal</a>
                                  <a class='%s' href='/log'> log</a>
                                  <a class='%s' href='/wiki'> wiki</a>
                                  <a href='/about'> about</a>
                                </div>
                                <div class='clearfix'></div>
                              </header>

                              <div class='page-header'>
                                <div class='page-meta'>%s</div>
                                <h1>%%t</h1>
                              </div>")
        (postamble "<footer id='footer'></footer>")
        (root-url "https://lepisma.github.io/")
        (output-dir (concat user-project-dir "lepisma.github.io-deploy/")))
    (setq pile-serve-dir output-dir
          pile-projects
          (list (pile-project :name "wiki"
                              :root-url root-url
                              :base-url "wiki"
                              :input-dir (concat user-project-dir "lepisma.github.io/wiki")
                              :output-dir (concat output-dir "wiki")
                              :type 'wiki
                              :postamble postamble
                              :preamble (format preamble-template "" "" "" "active" "Last modified: %d %C"))
                (pile-project :name "blog"
                              :root-url root-url
                              :base-url ""
                              :input-dir (concat user-project-dir "lepisma.github.io/blog")
                              :output-dir output-dir
                              :type 'blog
                              :postamble postamble
                              :preamble (format preamble-template "active" "" "" "" "%d"))
                (pile-project :name "journal"
                              :root-url root-url
                              :base-url "journal"
                              :input-dir (concat user-project-dir "lepisma.github.io/journal")
                              :output-dir (concat output-dir "journal")
                              :type 'blog
                              :postamble postamble
                              :preamble (format preamble-template "" "active" "" "" "%d"))
                (pile-project :name "log"
                              :root-url root-url
                              :base-url "log"
                              :input-dir (concat user-project-dir "lepisma.github.io/log")
                              :output-dir (concat output-dir "log")
                              :type 'blog
                              :postamble postamble
                              :preamble (format preamble-template "" "" "active" "" "%d"))
                (pile-project :name "assets"
                              :root-url root-url
                              :input-dir (concat user-project-dir "lepisma.github.io/assets")
                              :output-dir (concat output-dir "assets")
                              :type 'static)
                (pile-project :name "misc"
                              :root-url root-url
                              :base-url ""
                              :input-dir (concat user-project-dir "lepisma.github.io/misc")
                              :output-dir output-dir
                              :type 'blog
                              :postamble ""
                              :preamble "")))
    ;; Setup notes here to get the wiki files in agenda
    (r-org-setup-notes)
    (pile-setup)
    (add-hook 'pile-pre-publish-hook #'pile-hooks-pre-add-bc)
    (add-hook 'pile-pre-publish-hook #'pile-hooks-pre-add-cids)
    (add-hook 'pile-pre-publish-hook #'pile-hooks-pre-add-date)
    (add-hook 'pile-pre-publish-hook #'pile-hooks-pre-add-dropcap)
    (add-hook 'pile-pre-publish-hook #'pile-hooks-pre-add-tags)
    (add-hook 'pile-post-publish-hook #'pile-hooks-post-clear-cids)
    (add-hook 'pile-post-publish-hook #'pile-hooks-post-generate-atom)
    (add-hook 'pile-post-publish-hook #'pile-hooks-post-generate-archive)
    (add-hook 'pile-post-publish-hook #'pile-hooks-post-stringify-title)))

(rpkg pretty-mode
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

(rpkg (r-kv :location local)
  :config
  (setq r-kv-file (concat user-layer-dir "rkv.el")))

(rpkg (read-lyrics :location (recipe :fetcher github :repo "lepisma/read-lyrics.el"))
  :after (s spotify levenshtein))

(rpkg realgud
  :config
  (setq realgud:pdb-command-name "python -m pdb"))

(rpkg (r-ligatures :location local)
  :after r-utils
  :config
  (r-ligatures-setup-general)
  (r-ligatures-setup-ess))

(rpkg (r-mu4e :location local)
  :after (auth-source mu4e openwith message mml cl-macs s))

(rpkg (r-org :location local)
  :after (org pile)
  :config
  (r-org-setup-general)
  ;; Notes setup is done after pile
  ;; (r-org-setup-notes)
  (r-org-setup-babel)
  (r-org-setup-tex))

(rpkg (r-ui :location local)
  :after r-utils
  :config
  (r-ui-setup))

(rpkg (r-utils :location local))

(rpkg sage-shell-mode
  :config
  (sage-shell:define-alias)
  (setq sage-shell-view-default-resolution 150)
  (add-hook 'sage-shell-mode-hook #'eldoc-mode)
  (add-hook 'sage-shell:sage-mode-hook #'eldoc-mode)
  (add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode))

(rpkg shell-switcher
  :config (setq shell-switcher-mode t)
  :bind (("C-'" . shell-switcher-switch-buffer)
         ("C-\"" . shell-switcher-new-shell)))

(rpkg snakemake-mode)

(rpkg solaire-mode
  :hook ((prog-mode . turn-on-solaire-mode)
         (minibuffer-setup . solaire-mode-in-minibuffer)
         (ediff-prepare-buffer . solaire-mode)))

(rpkg (spaceline-all-the-icons :location local)
  :after spaceline
  :config
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-ati)))))

(rpkg swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(rpkg switch-window
  :config
  (global-set-key (kbd "C-x o") 'switch-window)
  (setq switch-window-shortcut-style 'qwerty
        switch-window-qwerty-shortcuts '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o")
        switch-window-minibuffer-shortcut ?z))

(rpkg (viz :location local))

(rpkg vue-mode
  :mode ("\\.vue\\'" . vue-mode))

(rpkg unicode-fonts
  :config
  (unicode-fonts-setup))

(rpkg (w :location (recipe :fetcher github :repo "lepisma/w.el")))

(rpkg writegood-mode)
