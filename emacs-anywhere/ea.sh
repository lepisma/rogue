#!/usr/bin/env bash

window=$( xdotool getactivewindow )

bash $HOME/.emacs.d/private/rogue/emacs-anywhere/emacstask.sh
xsel -b < $HOME/ea.clipboard
xdotool windowactivate --sync $window key --clearmodifiers ctrl+v
