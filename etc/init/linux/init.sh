#!/bin/bash

# zplug
if [ ! -d ~/.zplug ]; then
    git clone https://github.com/zplug/zplug ~/.zplug
fi

# nodebrew
if [ ! -d ~/.nodebrew ]; then
    curl -L git.io/nodebrew | perl - setup
fi

# rbenv
if [ ! -d ~/.rbenv ]; then
    git clone https://github.com/rbenv/rbenv.git ~/.rbenv
    ~/.rbenv/bin/rbenv init
    git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build
fi

# phpbrew
if [ ! -d ~/.phpbrew ]; then
  curl -L -O https://github.com/phpbrew/phpbrew/raw/master/phpbrew
  chmod +x phpbrew
  mv phpbrew ~/bin
  phpbrew init
fi

# pyenv
if [ ! -d ~/.pyenv ]; then
    git clone https://github.com/yyuu/pyenv.git ~/.pyenv
fi

