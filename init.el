(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

(require 'r-base)
(require 'r-writing)
(require 'r-programming)
(require 'r-planning)

;; Only load if mu4e is installed
(when (file-exists-p "/usr/share/emacs/site-lisp/mu4e")
  (require 'r-mail))

(require 'r-apps)

(require 'r-themes)
