# Check if zplug is installed
[[ -d ~/.zplug ]] || {
  curl -fLo ~/.zplug/zplug --create-dirs https://git.io/zplug
  source ~/.zplug/zplug && zplug update --self
}

# Essential
source ~/.zplug/zplug

zplug "supercrabtree/k"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-syntax-highlighting", nice:10

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

# 644 permission
umask 022

WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# completions

#fpath=(~/.zplug/repos/zsh-users/zsh-completions/src $fpath)

zstyle ':completion:*' group-name ''
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:descriptions' format '%d'
zstyle ':completion:*:options' verbose yes
zstyle ':completion:*:values' verbose yes
zstyle ':completion:*:options' prefix-needed yes

autoload -Uz compinit; compinit

zstyle ':completion:*:processes' command "ps -u $USER -o pid,stat,%cpu,%mem,cputime,command"

# colors

# Color settings for zsh complete candidates
case "${OSTYPE}" in
freebsd*|darwin*)
  alias la="ls -laG"
  alias ll="ls -lG"
  alias ls="ls -G"
  ;;
linux*)
  alias la='ls -aF --show-control-chars --color=always'
  alias ll='ls -lF --show-control-chars --color=always'
  alias ls='ls -F --show-control-chars --color=always'
  ;;
esac

export LSCOLORS=ExFxCxdxBxegedabagacad
export LS_COLORS='di=01;34:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
zstyle ':completion:*' list-colors 'di=;34;1' 'ln=;36;1' 'so=;32;1' 'ex=31;1' 'bd=46;34' 'cd=43;34'

# use prompt colors feature
autoload -U colors
colors

autoload -Uz vcs_info
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "%F{yellow}!"
zstyle ':vcs_info:git:*' unstagedstr "%F{red}+"
zstyle ':vcs_info:*' formats "%F{green}%c%u[%b]%f"
zstyle ':vcs_info:*' actionformats '[%b|%a]'
precmd () { vcs_info }

RPROMPT='${vcs_info_msg_0_}'

PROMPT="[%n] %{${fg[yellow]}%}%~%{${reset_color}%}
%(?.%{$fg[green]%}.%{$fg[blue]%})%(?!凸 <!ζ'ヮ'%)ζ <)%{${reset_color}%} "

PROMPT2='[%n]> '

SPROMPT="%{$fg[red]%}%{$suggest%}∞のワの? < %B%r%b %{$fg[red]%}is collect? [yes!(y), no!(n),a,e]:${reset_color} "

# options
setopt correct
setopt re_match_pcre
setopt prompt_subst

# alias
alias dclean='docker rmi $(docker images | awk "/^<none>/ { print $3 }")'

# environment
export LANG=ja_JP.UTF-8
export PATH=/usr/local/sbin:$PATH
