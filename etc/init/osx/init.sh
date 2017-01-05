#!/bin/bash

# Homebrew
if ! which brew >/dev/null 2>&1; then
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# zplug
if [ ! -d ~/.zplug ]; then
    brew install zplug
fi

# nodebrew
if [ ! -d ~/.nodebrew ]; then
    curl -L git.io/nodebrew | perl - setup
fi
