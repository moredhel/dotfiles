# sweet emacs
bindkey -e

RED="\[\033[0;31m\]"
PINK="\[\033[1;31m\]"
YELLOW="\[\033[1;33m\]"
GREEN="\[\033[0;32m\]"
LT_GREEN="\[\033[1;32m\]"
BLUE="\[\033[0;34m\]"
WHITE="\[\033[1;37m\]"
PURPLE="\[\033[1;35m\]"
CYAN="\[\033[1;36m\]"
BROWN="\[\033[0;33m\]"
COLOR_NONE="\[\033[0m\]"

# make this only set on OSX
fpath=(/usr/local/share/zsh-completions $fpath)


# core alias'
alias ls="ls -G"
alias _="sudo"
alias g="git"
alias e="emacsclient -n"
alias be='bundle exec'
alias f="git-flow"
alias grep='grep --color=auto' # Always highlight grep search term
alias ping='ping -c 5'      # Pings with 5 packets, not unlimited
alias df='df -h'            # Disk free, in gigabytes, not bytes
alias du='du -h -c'         # Calculate total disk usage for a folder
alias clr='clear;echo "Currently logged in on $(tty), as $(whoami) in directory $(pwd)."'

# utility alias'
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
alias localip="ipconfig getifaddr en0"

# some useful paths
export PATH=./vendor/bundler/bin:./node_modules/.bin:~/.bin:~/.cabal/bin:$PATH
export PATH=$PATH:/usr/local/opt/go/libexec/bin:$HOME/perl5/bin
export PATH=./bin:$PATH

# utility functions
ghget () {
    # input: rails/rails
    USER=$(echo $@ | tr "/" " " | awk '{print $1}')
    REPO=$(echo $@ | tr "/" " " | awk '{print $2}')
    DIR="$HOME/src/github.com/$USER"
    mkdir -p $DIR
    cd $DIR
    git clone "https://github.com/"$@ && \
    cd $REPO
  }

# system env. variables
export EDITOR="vim"
export GPG_TTY=`tty`

# override cd function to save last visited location
function cd {
    builtin cd $1
    echo `pwd` > ~/.last_location
}

# zsh custom options
autoload -U select-word-style
select-word-style bash

unsetopt beep
unsetopt correct_all
unset GREP_OPTIONS

# History
setopt inc_append_history share_history
export HISTFILE="$HOME/.zsh_history"
export SAVEHIST=10000000
# custom history manouvers
bindkey "${key[Up]}" up-line-or-local-history
bindkey "${key[Down]}" down-line-or-local-history

up-line-or-local-history() {
    zle set-local-history 1
    zle up-line-or-history
    zle set-local-history 0
}
zle -N up-line-or-local-history
down-line-or-local-history() {
    zle set-local-history 1
    zle down-line-or-history
    zle set-local-history 0
}
zle -N down-line-or-local-history

# fish autocompletion
# https://github.com/zsh-users/zsh-autosuggestions
source ~/.zsh/zsh-autocompletions/zsh-autosuggestions.zsh
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=250'

# setup powerline
export POWERLINE_LOCATION="/usr/local/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh"
if [ -f `which powerline-daemon` ]; then
    powerline-daemon -q
fi

# go config...
export WORKON_HOME=$HOME/.envs


# source local config.
source $HOME/.zshrc_local

# this is after the local config, allowing customization of location
if [[ -r $POWERLINE_LOCATION ]]; then
    source $POWERLINE_LOCATION
fi

if [ $TERM = "eterm-color" ]; then
  # prompt for emacs (width sensitive)
  PS1='\u@\h:\w\$ '
fi
# lastly cd to previous location
cd `cat ~/.last_location`
