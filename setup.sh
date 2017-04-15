#!/usr/bin/env bash

cd ~/
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
git clone git@github.com:lepisma/rogue ~/.emacs.d/private/rogue
ln -s ~/.emacs.d/private/rogue/.spacemacs ./
