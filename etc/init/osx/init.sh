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

# phpbrew
if [ ! -d ~/.phpbrew ]; then
  curl -L -O https://github.com/phpbrew/phpbrew/raw/master/phpbrew
  chmod +x phpbrew
  mv phpbrew ~/bin
  phpbrew init
  phpbrew lookup-prefix homebrew
fi
