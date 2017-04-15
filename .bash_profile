# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# User specific environment and startup programs
export LANG=en_US.UTF-8
export PATH="$HOME/bin:$PATH"

# Settings for docker compose
export COMPOSE_CONVERT_WINDOWS_PATHS=0

# Add executables in /usr/local/sbin
export PATH="/usr/local/sbin:$PATH"

# Settings for nodebrew
export PATH="$HOME/.nodebrew/current/bin:$PATH"

# Settings for golang
export GOPATH="$HOME/go"
export GOBIN="$GOPATH/bin"
export PATH="$GOBIN:$PATH"