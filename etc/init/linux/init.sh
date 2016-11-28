#!/bin/bash

# zplug
if [ ! -d ~/.zplug ]; then
    git clone https://github.com/zplug/zplug ~/.zplug
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
fi

