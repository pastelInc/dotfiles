#!/bin/bash

# zplug
if [ ! -d ~/.zplug ]; then
    git clone https://github.com/zplug/zplug ~/.zplug
fi

# nodebrew
if [ ! -d ~/.nodebrew ]; then
    curl -L git.io/nodebrew | perl - setup
fi
