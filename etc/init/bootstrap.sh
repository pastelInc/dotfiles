# zplug
if [ ! -d ~/.zplug ]; then
    curl -sL zplug.sh/installer | zsh
fi
# Neobundle
if [ ! -d ~/.vim/bundle ]; then
    git clone https://github.com/Shougo/neobundle.vim.git ~/.vim/bundle/neobundle.vim
fi
