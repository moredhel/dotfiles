#oh-my-zsh stuff
CASE_SENSITIVE="true"
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="af-magic"
plugins=(git git-flow zsh-syntax-highlighting)
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)


ncmpcppShow() { BUFFER="ncmpcpp"; zle accept-line; }
zle -N ncmpcppShow
bindkey '^[\' ncmpcppShow
alias _="sudo"
alias aoeu="kinit s1249759@INF.ED.AC.UK"
alias snth="ncmpcpp"
alias cdp="cd ~/documents/lecture_notes"
alias t="task"
alias g="git"
alias em="emacsclient -t"
alias es="emacsclient"
alias g="git"
alias ipy="python -c 'import IPython; IPython.terminal.ipapp.launch_new_instance()'"
source $ZSH/oh-my-zsh.sh
[ -n "$XTERM_VERSION" ] && transset-df -a >/dev/null
export EDITOR="mvim"
export GPG_TTY=`tty`
export RBENV_ROOT=/usr/local/var/rbenv
export WORKON_HOME=$HOME/.envs
unset GREP_OPTIONS
export PATH=./vendor/bundler/bin:~/.bin/:~/.cabal/bin:$PATH

function cd {
    builtin cd $1
    echo `pwd` > ~/.last_location
}
cd `cat ~/.last_location`
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/local/bin/virtualenvwrapper.sh
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

function test {

}
export DOCKER_TLS_VERIFY=1
export DOCKER_HOST=tcp://192.168.59.103:2376
export DOCKER_CERT_PATH=/Users/moredhel/.boot2docker/certs/boot2docker-vm
export PIP_REQUIRE_VIRTUALENV=true
