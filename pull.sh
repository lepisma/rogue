# Pull current .spacemacs file without customization

sed -n "/Do not write/q;p" ~/.spacemacs > .spacemacs
