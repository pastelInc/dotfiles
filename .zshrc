# Check if zplug is installed
if [[ ! -d ~/.zplug ]]; then
    git clone https://github.com/zplug/zplug ~/.zplug
    source ~/.zplug/init.zsh && zplug update --self
fi

# Essential
source ~/.zplug/init.zsh

zplug "mafredri/zsh-async", from:github
zplug "sindresorhus/pure", use:pure.zsh, from:github, as:theme
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-syntax-highlighting"
zplug "tj/git-extras", use:"etc/*.zsh", hook-build:"PREFIX=$ZPLUG_HOME make install"

# Supports oh-my-zsh plugins and the like
zplug "plugins/git", from:oh-my-zsh
zplug "plugins/common-aliases", from:oh-my-zsh
zplug "plugins/emacs", from:oh-my-zsh
zplug "plugins/node", from:oh-my-zsh
zplug "plugins/npm", from:oh-my-zsh
zplug "plugins/kubectl", from:oh-my-zsh
zplug "plugins/docker", from:oh-my-zsh
zplug "plugins/docker-compose", from:oh-my-zsh

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

# The next line updates PATH for the Google Cloud SDK.
if [ -f $HOME'/google-cloud-sdk/path.zsh.inc' ]; then source $HOME'/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f $HOME'/google-cloud-sdk/completion.zsh.inc' ]; then source $HOME'/google-cloud-sdk/completion.zsh.inc'; fi

# Load zsh local settings
[ -f ~/.zshrc.local ] && source ~/.zshrc.local
