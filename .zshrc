#oh-my-zsh config
CASE_SENSITIVE="true"
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="robbyrussell"
plugins=(git-flow git zsh-syntax-highlighting)
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)

# make this only set on OSX
fpath=(/usr/local/share/zsh-completions $fpath)


# core alias'
alias _="sudo"
alias g="git"
alias e="emacsclient -n"
alias be='bundle exec'
alias f="git-flow"

# utility alias'
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
alias localip="ipconfig getifaddr en0"

# some useful paths
export PATH=./vendor/bundler/bin:./node_modules/.bin:~/.bin:~/.cabal/bin:$PATH
export PATH=$PATH:/usr/local/opt/go/libexec/bin:$HOME/perl5/bin
export PATH=./bin:$PATH

# should probs figure out what this does...
[ -n "$XTERM_VERSION" ] && transset-df -a >/dev/null

# system env. variables
export EDITOR="vim"
export GPG_TTY=`tty`
unset GREP_OPTIONS

# override cd function to save last visited location
function cd {
    builtin cd $1
    echo `pwd` > ~/.last_location
}

# create an emacs opener.
function ec {
  which osascript > /dev/null 2>&1 && osascript -e 'tell application "Emacs" to activate'
  emacsclient -nc $@
}

export WORKON_HOME=$HOME/.envs

unsetopt correct_all

# source oh-my-zsh
# TODO: reduce depenency on oh-my-zsh.
source $ZSH/oh-my-zsh.sh
# source local config.
source $HOME/.zshrc_local

# lastly cd to previous location
cd `cat ~/.last_location`
