;; Early init

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))
(setenv "LSP_USE_PLISTS" "true")
