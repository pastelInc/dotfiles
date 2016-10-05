# zplug
if [ ! -d ~/.zplug ]; then
    export ZPLUG_HOME=~/.zplug
    git clone https://github.com/zplug/zplug $ZPLUG_HOME
    source $ZPLUG_HOME/init.zsh
fi
# Neobundle
if [ ! -d ~/.vim/bundle ]; then
    git clone https://github.com/Shougo/neobundle.vim.git ~/.vim/bundle/neobundle.vim
fi
