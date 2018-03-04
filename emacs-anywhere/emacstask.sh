#!/usr/bin/env bash

ELISP=$( cat $HOME/.emacs.d/private/rogue/emacs-anywhere/emacs-anywhere.el )

emacsclient -a '' -c -e "(progn $ELISP)"
