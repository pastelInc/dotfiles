# Check if zplug is installed
if [[ ! -d ~/.zplug ]]; then
    git clone https://github.com/zplug/zplug ~/.zplug
    source ~/.zplug/init.zsh && zplug update --self
fi

# Essential
source ~/.zplug/init.zsh

zplug "direnv/direnv", as:command, use:"direnv", hook-build:"make"
zplug "sindresorhus/pure", use:"*.zsh", as:theme
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-syntax-highlighting"

# Supports oh-my-zsh plugins and the like
zplug "plugins/git",   from:oh-my-zsh
zplug "plugins/common-aliases",   from:oh-my-zsh
zplug "plugins/emacs",   from:oh-my-zsh

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    else
        echo
    fi
fi

# Then, source plugins and add commands to $PATH
zplug load --verbose

# Emacs daemon killing command
alias ekill='emacsclient -e "(kill-emacs)"'

# Setup direnv
if type direnv >/dev/null 2>&1; then
    eval "$(direnv hook zsh)"
fi

# Load zsh local settings
[ -f ~/.zshrc.local ] && source ~/.zshrc.local
