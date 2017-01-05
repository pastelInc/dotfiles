#!/bin/bash

# Neobundle
if [ ! -d ~/.vim/bundle ]; then
    git clone https://github.com/Shougo/neobundle.vim.git ~/.vim/bundle/neobundle.vim
fi

# Cask
if [ ! -d ~/.cask ]; then
    curl -fsSkL https://raw.github.com/cask/cask/master/go | python
fi

case "${OSTYPE}" in
    darwin*)
        bash "${DOTPATH}/etc/init/osx/init.sh"
        ;;
    linux*)
        bash "${DOTPATH}/etc/init/linux/init.sh"
        ;;
esac
