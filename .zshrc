# sweet emacs
bindkey -e

setopt PROMPT_SUBST
autoload -U promptinit
autoload -U colors && colors
# prompt setup
export ZSH_THEME_GIT_PROMPT_PREFIX=""
export ZSH_THEME_GIT_PROMPT_SUFFIX=""
export ZSH_THEME_GIT_PROMPT_DIRTY=""
export ZSH_THEME_GIT_PROMPT_CLEAN=""
export ZSH_THEME_GIT_PROMPT_ADDED="%{$fg[cyan]%} ✈"
export ZSH_THEME_GIT_PROMPT_MODIFIED="%{$fg[yellow]%} ✭"
export ZSH_THEME_GIT_PROMPT_DELETED="%{$fg[red]%} ✗"
export ZSH_THEME_GIT_PROMPT_RENAMED="%{$fg[blue]%} ➦"
export ZSH_THEME_GIT_PROMPT_UNMERGED="%{$fg[magenta]%} ✂"
export ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[grey]%} ✱"



function parse_git_dirty() {
  local STATUS=''
  local FLAGS
  FLAGS=('--porcelain')
  if [[ "$(command git config --get oh-my-zsh.hide-dirty)" != "1" ]]; then
    if [[ $POST_1_7_2_GIT -gt 0 ]]; then
      FLAGS+='--ignore-submodules=dirty'
    fi
    if [[ "$DISABLE_UNTRACKED_FILES_DIRTY" == "true" ]]; then
      FLAGS+='--untracked-files=no'
    fi
    STATUS=$(command git status ${FLAGS} 2> /dev/null | tail -n1)
  fi
  if [[ -n $STATUS ]]; then
    echo "$ZSH_THEME_GIT_PROMPT_DIRTY"
  else
    echo "$ZSH_THEME_GIT_PROMPT_CLEAN"
  fi
}

function git_prompt_info() {
  local ref
  if [[ "$(command git config --get oh-my-zsh.hide-status 2>/dev/null)" != "1" ]]; then
    ref=$(command git symbolic-ref HEAD 2> /dev/null) || \
    ref=$(command git rev-parse --short HEAD 2> /dev/null) || return 0
    echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$(parse_git_dirty)$ZSH_THEME_GIT_PROMPT_SUFFIX"
  fi
}

function git_prompt_status() {
  local INDEX STATUS
  INDEX=$(command git status --porcelain -b 2> /dev/null)
  STATUS=""
  if $(echo "$INDEX" | command grep -E '^\?\? ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_UNTRACKED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^A  ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_ADDED$STATUS"
  elif $(echo "$INDEX" | grep '^M  ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_ADDED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^ M ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
  elif $(echo "$INDEX" | grep '^AM ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
  elif $(echo "$INDEX" | grep '^ T ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^R  ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_RENAMED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^ D ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_DELETED$STATUS"
  elif $(echo "$INDEX" | grep '^D  ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_DELETED$STATUS"
  elif $(echo "$INDEX" | grep '^AD ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_DELETED$STATUS"
  fi
  if $(command git rev-parse --verify refs/stash >/dev/null 2>&1); then
    STATUS="$ZSH_THEME_GIT_PROMPT_STASHED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^UU ' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_UNMERGED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^## .*ahead' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_AHEAD$STATUS"
  fi
  if $(echo "$INDEX" | grep '^## .*behind' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_BEHIND$STATUS"
  fi
  if $(echo "$INDEX" | grep '^## .*diverged' &> /dev/null); then
    STATUS="$ZSH_THEME_GIT_PROMPT_DIVERGED$STATUS"
  fi
  echo $STATUS
}

function my_test() {
  echo "Hello"
}

export PROMPT="%{$fg[magenta]%}[%c] %{$reset_color%}"
export RPROMPT="%{$fg[magenta]%}$(git_prompt_info)%{$reset_color%} $(git_prompt_status)%{$reset_color%}"

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
autoload -Uz compinit && compinit # command completion
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
# bindkey "${key[Up]}" up-line-or-local-history
# bindkey "${key[Down]}" down-line-or-local-history

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

# go config...
export WORKON_HOME=$HOME/.envs


# source local config.
source $HOME/.zshrc_local

# this is after the local config, allowing customization of location
# if [[ -r $POWERLINE_LOCATION ]]; then
    # source $POWERLINE_LOCATION
# fi

if [ $TERM = "eterm-color" ]; then
  # prompt for emacs (width sensitive)
  PS1='\u@\h:\w\$ '
fi
# lastly cd to previous location
cd "`cat ~/.last_location`"

export PROMPT='%{$fg[magenta]%}[%c] %{$reset_color%}'
export RPROMPT='%{$fg[magenta]%}$(git_prompt_info)%{$reset_color%} $(git_prompt_status)%{$reset_color%}'
