#!/bin/bash

# Neobundle
if [ ! -d ~/.vim/bundle ]; then
    git clone https://github.com/Shougo/neobundle.vim.git ~/.vim/bundle/neobundle.vim
fi

case "${OSTYPE}" in
    darwin*)
        bash "${DOTPATH}/etc/init/osx/init.sh"
        ;;
    linux*)
        bash "${DOTPATH}/etc/init/linux/init.sh"
        ;;
esac
