#!/usr/bin/env bash

git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
git clone git@github.com:lepisma/rogue ~/.emacs.d/private/rogue
ln -s ~/.emacs.d/private/rogue/.spacemacs ~/.spacemacs
touch ~/.emacs-custom.el

# Secret files stubs
mkdir -p ~/.emacs.d/private/rogue/secrets
touch ~/.emacs.d/private/rogue/secrets/slack.el
touch ~/.emacs.d/private/rogue/secrets/wolfram.el
