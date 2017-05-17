;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(;; Pure languages
     c-c++
     common-lisp
     emacs-lisp
     ess
     extra-langs
     haskell
     html
     java
     javascript
     nim
     (python :variables python-enable-yapf-format-on-save t)
     racket
     scheme
     (typescript :variables typescript-fmt-on-save t)
     ;; Other languages / assists
     bibtex
     csv
     graphviz
     (latex :variables
            latex-enable-folding t
            latex-enable-auto-fill t)
     (markdown :variables markdown-live-preview-engine 'vmd)
     (org :variables
          org-enable-github-support t
          org-enable-reveal-js-support t)
     shell
     shell-scripts
     sql
     yaml
     ;; Everything else
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-private-snippets-directory "~/.emacs.d/private/rogue/snippets")
     better-defaults
     colors
     dash
     deft
     (elfeed :variables
             rmh-elfeed-org-files '("~/.emacs.d/private/rogue/feeds.org")
             elfeed-enable-web-interface t)
     emoji
     games
     git
     github
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     imenu-list
     nlinum
     pandoc
     pdf-tools
     (ranger :variables ranger-show-preview t)
     restclient
     rogue
     selectric
     semantic
     slack
     (spell-checking :variables spell-checking-enable-by-default nil)
     spotify
     syntax-checking
     theming
     typography
     twitter
     (version-control :variables
                      version-control-diff-tool 'git-gutter
                      version-control-global-margin t
                      version-control-diff-side 'left)
     xkcd)
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(vi-tilde-fringe)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style '(hybrid :variables
                                       hybrid-mode-default-state 'hybrid)
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(doom-molokai)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state nil
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 12
                               :weight regular
                               :width normal
                               :powerline-scale 1.0)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts t
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header t
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
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
  (defconst user-journal-dir (file-name-as-directory (getenv "JOURNAL_DIR")))
  (defconst user-diary-dir (file-name-as-directory (concat user-journal-dir "diary")))
  (defconst user-project-dir (file-name-as-directory (getenv "PROJECTS_DIR")))
  (defconst user-project-files
    `(,(concat user-journal-dir "projects.org")
      ,(concat user-project-dir "dev/index.org")))
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

  ;; Fira Code ligatures
  ;; This works when using emacs --daemon + emacsclient
  (add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
  ;; This works when using emacs without server/client
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
  (defconst fira-code-font-lock-keywords-alist
    (mapcar (lambda (regex-char-pair)
              `(,(car regex-char-pair)
                (0 (prog1 ()
                     (compose-region (match-beginning 1)
                                     (match-end 1)
                                     ;; The first argument to concat is a string containing a literal tab
                                     ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
            '(("\\(www\\)"                   #Xe100)
              ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
              ("\\(\\*\\*\\*\\)"             #Xe102)
              ("\\(\\*\\*/\\)"               #Xe103)
              ("\\(\\*>\\)"                  #Xe104)
              ("[^*]\\(\\*/\\)"              #Xe105)
              ("\\(\\\\\\\\\\)"              #Xe106)
              ("\\(\\\\\\\\\\\\\\)"          #Xe107)
              ("\\({-\\)"                    #Xe108)
              ("\\(::\\)"                    #Xe10a)
              ("\\(:::\\)"                   #Xe10b)
              ("[^=]\\(:=\\)"                #Xe10c)
              ("\\(!!\\)"                    #Xe10d)
              ("\\(!=\\)"                    #Xe10e)
              ("\\(!==\\)"                   #Xe10f)
              ("\\(-}\\)"                    #Xe110)
              ("\\(--\\)"                    #Xe111)
              ("\\(---\\)"                   #Xe112)
              ("\\(-->\\)"                   #Xe113)
              ("[^-]\\(->\\)"                #Xe114)
              ("\\(->>\\)"                   #Xe115)
              ("\\(-<\\)"                    #Xe116)
              ("\\(-<<\\)"                   #Xe117)
              ("\\(-~\\)"                    #Xe118)
              ("\\(#{\\)"                    #Xe119)
              ("\\(#\\[\\)"                  #Xe11a)
              ("\\(##\\)"                    #Xe11b)
              ("\\(###\\)"                   #Xe11c)
              ("\\(####\\)"                  #Xe11d)
              ("\\(#(\\)"                    #Xe11e)
              ("\\(#\\?\\)"                  #Xe11f)
              ("\\(#_\\)"                    #Xe120)
              ("\\(#_(\\)"                   #Xe121)
              ("\\(\\.-\\)"                  #Xe122)
              ("\\(\\.=\\)"                  #Xe123)
              ("\\(\\.\\.\\)"                #Xe124)
              ("\\(\\.\\.<\\)"               #Xe125)
              ("\\(\\.\\.\\.\\)"             #Xe126)
              ("\\(\\?=\\)"                  #Xe127)
              ("\\(\\?\\?\\)"                #Xe128)
              ("\\(;;\\)"                    #Xe129)
              ("\\(/\\*\\)"                  #Xe12a)
              ("\\(/\\*\\*\\)"               #Xe12b)
              ("\\(/=\\)"                    #Xe12c)
              ("\\(/==\\)"                   #Xe12d)
              ("\\(/>\\)"                    #Xe12e)
              ("\\(//\\)"                    #Xe12f)
              ("\\(///\\)"                   #Xe130)
              ("\\(&&\\)"                    #Xe131)
              ("\\(||\\)"                    #Xe132)
              ("\\(||=\\)"                   #Xe133)
              ("[^|]\\(|=\\)"                #Xe134)
              ("\\(|>\\)"                    #Xe135)
              ("\\(\\^=\\)"                  #Xe136)
              ("\\(\\$>\\)"                  #Xe137)
              ("\\(\\+\\+\\)"                #Xe138)
              ("\\(\\+\\+\\+\\)"             #Xe139)
              ("\\(\\+>\\)"                  #Xe13a)
              ("\\(=:=\\)"                   #Xe13b)
              ("[^!/]\\(==\\)[^>]"           #Xe13c)
              ("\\(===\\)"                   #Xe13d)
              ("\\(==>\\)"                   #Xe13e)
              ("[^=]\\(=>\\)"                #Xe13f)
              ("\\(=>>\\)"                   #Xe140)
              ("\\(<=\\)"                    #Xe141)
              ("\\(=<<\\)"                   #Xe142)
              ("\\(=/=\\)"                   #Xe143)
              ("\\(>-\\)"                    #Xe144)
              ("\\(>=\\)"                    #Xe145)
              ("\\(>=>\\)"                   #Xe146)
              ("[^-=]\\(>>\\)"               #Xe147)
              ("\\(>>-\\)"                   #Xe148)
              ("\\(>>=\\)"                   #Xe149)
              ("\\(>>>\\)"                   #Xe14a)
              ("\\(<\\*\\)"                  #Xe14b)
              ("\\(<\\*>\\)"                 #Xe14c)
              ("\\(<|\\)"                    #Xe14d)
              ("\\(<|>\\)"                   #Xe14e)
              ("\\(<\\$\\)"                  #Xe14f)
              ("\\(<\\$>\\)"                 #Xe150)
              ("\\(<!--\\)"                  #Xe151)
              ("\\(<-\\)"                    #Xe152)
              ("\\(<--\\)"                   #Xe153)
              ("\\(<->\\)"                   #Xe154)
              ("\\(<\\+\\)"                  #Xe155)
              ("\\(<\\+>\\)"                 #Xe156)
              ("\\(<=\\)"                    #Xe157)
              ("\\(<==\\)"                   #Xe158)
              ("\\(<=>\\)"                   #Xe159)
              ("\\(<=<\\)"                   #Xe15a)
              ("[^-=]\\(<<\\)"               #Xe15c)
              ("\\(<<-\\)"                   #Xe15d)
              ("\\(<<=\\)"                   #Xe15e)
              ("\\(<<<\\)"                   #Xe15f)
              ("\\(<~\\)"                    #Xe160)
              ("\\(<~~\\)"                   #Xe161)
              ("\\(</\\)"                    #Xe162)
              ("\\(</>\\)"                   #Xe163)
              ("\\(~@\\)"                    #Xe164)
              ("\\(~-\\)"                    #Xe165)
              ("\\(~=\\)"                    #Xe166)
              ("\\(~>\\)"                    #Xe167)
              ("[^<]\\(~~\\)"                #Xe168)
              ("\\(~~>\\)"                   #Xe169)
              ("\\(%%\\)"                    #Xe16a)
              ("[^:=]\\(:\\)[^:=]"           #Xe16c)
              ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
              ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

  (defun add-fira-code-symbol-keywords ()
    (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))
  (add-hook 'prog-mode-hook 'add-fira-code-symbol-keywords)
  (add-hook
   'python-mode-hook
   (lambda ()
     (mapc (lambda (pair) (push pair prettify-symbols-alist))
           '(("not"      . #x2757)
             ("in"       . #x2208)
             ("not in"   . #x2209)
             ("return"   . #x27fc)
             ("yield"    . #x27fb)
             ("for"      . #x2200)
             ("int"      . #x2124)
             ("float"    . #x211d)
             ("True"     . #x1d54b)
             ("False"    . #x1d53d)))))
  (add-hook
   'ess-mode-hook
   (lambda ()
     (progn
       (pretty-mode -1)
       (push '("%>%" . ?|) prettify-symbols-alist))))
  (add-hook
   'inferior-ess-mode-hook
   (lambda ()
     (progn
       (pretty-mode -1)
       (push '("%>%" . ?|) prettify-symbols-alist))))

  (setq wdired-allow-to-change-permissions t)

  ;; Smaller fonts in echo area
  (with-current-buffer (get-buffer " *Echo Area 0*")
    (setq-local face-remapping-alist '((default (:height 0.9) ))))
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (set (make-local-variable 'face-remapping-alist) '((default :height 0.9)))))

  (global-company-mode)
  (blink-cursor-mode t)
  (setq-default cursor-in-non-selected-windows nil)
  (setq frame-title-format "")

  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)

  (setq neo-banner-message nil
        neo-mode-line-type 'none
        neo-autorefresh nil)

  ;; Hide mode line in few situations
  (with-current-buffer "*Messages*" (hidden-mode-line-mode +1))
  (advice-add 'helm-display-mode-line
              :override (lambda (source &optional force) (hidden-mode-line-mode +1)))

  (setq-default fringes-outside-margins t
                indicate-buffer-boundaries nil
                fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                                             fringe-indicator-alist))
  (setq flycheck-indication-mode nil)
  (define-fringe-bitmap 'git-gutter-fr:added
    [224 224 224 224 224 224 224 224 224 224 224 224 224
         224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224 224 224 224 224 224 224 224 224 224 224 224 224
         224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil nil 'center)
  (fringe-mode 3)

  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)
  (setq-default css-indent-offset 2)

  (setq-default js2-basic-offset 2)
  (setq-default js-indent-level 2)
  (setq-default js2-strict-missing-semi-warning nil)
  (setq-default js2-missing-semi-one-line-override nil)

  (setq-default typescript-indent-level 2)

  (setq deft-directory user-journal-dir)
  (setq deft-extensions '("org"))
  (setq deft-recursive t)

  (use-package em-tramp
    :config
    (setq eshell-prefer-lisp-functions t)
    (setq password-cache t)
    (setq password-cache-expiry 3600))

  (setq comint-scroll-show-maximum-output nil)
  (setq comint-input-ignoredups t)

  (setq inferior-lisp-program "ros -Q run")
  (slime-setup '(slime-asdf
                 slime-company
                 slime-fancy
                 slime-indentation
                 slime-sbcl-exts
                 slime-scratch
                 slime-tramp))
  (setq alert-default-style 'libnotify)
  (load-file (concat user-secrets-dir "slack.el"))

  ;; Delay slack
  (run-at-time "30 sec" nil 'slack-start)

  (setq tramp-default-method "ssh")

  (setq ranger-cleanup-eagerly t)
  (setq ranger-show-hidden t)
  (setq ranger-parent-depth 1)

  (defun ranger-custom-lhs ()
    (let* ((current-file (or (r--fget ranger-current-file) ""))
           (file-path (file-name-directory current-file)))
      (propertize (abbreviate-file-name file-path)
                  'face `(:inherit variable-pitch :height 1.2 :box (:line-width 4 :color "gray20")))))

  (setq ranger-header-func (lambda ()
                             (let* ((lhs (ranger-custom-lhs))
                                    (minimal (r--fget ranger-minimal))
                                    (used-length (length lhs))
                                    (fringe-gap (if (eq fringe-mode 0) 2 0))
                                    (total-window-width (+ 3 (if minimal
                                                                 (window-width)
                                                               (- (frame-width) fringe-gap))))
                                    (filler (make-string (max 0 (- total-window-width used-length)) (string-to-char " "))))
                               (concat lhs filler))))

  (setq nlinum-format " %d ")

  ;; Hooks
  (defun add-hooks (hooks fun)
    "Add FUN to all the HOOKS."
    (dolist (hook hooks)
      (add-hook hook fun)))

  (add-hooks '(text-mode-hook org-agenda-mode-hook)
             (lambda () (progn
                     (setq left-margin-width 2)
                     (set-window-buffer nil (current-buffer)))))

  (add-hooks '(text-mode-hook prog-mode-hook org-agenda-mode-hook ranger-mode-hook)
             (lambda () (progn
                     (setq line-spacing 0.1))))

  (add-hooks '(processing-compilation-mode-hook
               eshell-mode-hook
               neo-after-create-hook
               help-mode-hook
               compilation-mode-hook
               messages-buffer-mode-hook
               completion-list-mode-hook
               ranger-mode-hook
               ranger-parent-dir-hook
               ranger-preview-dir-hook
               magit-status-mode-hook)
             'hidden-mode-line-mode)

  (add-hook 'css-mode-hook (lambda () (rainbow-mode 1)))
  (add-hook 'text-mode-hook (lambda ()
                              (progn
                                (auto-fill-mode)
                                (spacemacs/disable-hl-line-mode))))
  (add-hook 'term-mode-hook 'toggle-truncate-lines)
  (add-hook 'prog-mode-hook 'nlinum-mode)

  ;; Org
  (setq bib-library "~/library.bib")
  (setq reftex-default-bibliography '(bib-library)
        org-ref-default-bibliography '(bib-library)
        bibtex-completion-bibliography bib-library)
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"))

  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (js . t)
     (latex . t)
     (python . t)
     (sh . t)
     (sqlite . t)))

  (setq spaceline-org-clock-p t)

  (with-eval-after-load 'org
    (setq org-startup-indented t)
    (setq org-clock-idle-time 5)
    (setq org-bullets-bullet-list '("#"))
    (setq org-ellipsis "  ")
    (setq org-pretty-entities t)
    (setq org-hide-emphasis-markers t)
    (setq org-agenda-block-separator ?─)
    (customize-set-variable 'org-modules
                            '(org-bibtex
                              org-docview
                              org-habit
                              org-info
                              org-w3m))
    ;; Avoid duplicating micro
    (setq org-tags-exclude-from-inheritance '("micro"))
    (setq org-stuck-projects
          '("+LEVEL=1/-DONE" ("*") ("active" "micro" "old") ""))
    (setq org-agenda-custom-commands
          '(("n" "Main agenda with micro tasks"
             ((tags "micro"
                    ((org-agenda-overriding-header "Micro tasks")))
              (agenda "")))
            ("p" "Projects to work on"
             ((tags "micro"
                    ((org-agenda-files user-project-files)
                     (org-agenda-overriding-header "Micro tasks")))
              (todo "TODO"
                    ((org-agenda-files user-project-files)
                     (org-agenda-overriding-header "Tasks")))
              (tags "active"
                    ((org-agenda-files user-project-files)
                     (org-agenda-overriding-header "Active projects")))
              (stuck ""
                     ((org-agenda-files user-project-files)
                      (org-agenda-overriding-header "Needs action")))
              (tags "old"
                    ((org-agenda-files user-project-files)
                     (org-agenda-overriding-header "Too old")))))
            ("d" "Upcoming deadlines" agenda ""
             ((org-agenda-entry-types '(:deadline))
              (org-deadline-warning-days 30)
              (org-agenda-time-grid nil)
              (org-agenda-overriding-header "Upcoming deadlines")))))))
