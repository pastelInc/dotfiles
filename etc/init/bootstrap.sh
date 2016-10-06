#!/bin/bash

# zplug
if [ ! -d ~/.zplug ]; then
    git clone https://github.com/zplug/zplug ~/.zplug
fi
# Neobundle
if [ ! -d ~/.vim/bundle ]; then
    git clone https://github.com/Shougo/neobundle.vim.git ~/.vim/bundle/neobundle.vim
fi

case "${OSTYPE}" in
    darwin*)
        ;;
    linux*)
        bash ./linux/init.sh
        ;;
esac
