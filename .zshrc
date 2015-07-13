#oh-my-zsh stuff
CASE_SENSITIVE="true"
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="af-magic"
plugins=(git-flow git zsh-syntax-highlighting)
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)

fpath=(/usr/local/share/zsh-completions $fpath)


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
alias be='bundle exec'

export PATH=./vendor/bundler/bin:./node_modules/.bin:~/.bin:~/.cabal/bin:$PATH
export PATH=$PATH:/usr/local/opt/go/libexec/bin:$HOME/perl5/bin
export PATH=./bin:$PATH

source $ZSH/oh-my-zsh.sh
[ -n "$XTERM_VERSION" ] && transset-df -a >/dev/null
export EDITOR="vim"
export GPG_TTY=`tty`
export WORKON_HOME=$HOME/.envs
unset GREP_OPTIONS
export GOPATH=$HOME/.gocode

function cd {
    builtin cd $1
    echo `pwd` > ~/.last_location
}
cd `cat ~/.last_location`
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

function test {

}
export DOCKER_HOST=tcp://192.168.59.104:2376
export DOCKER_CERT_PATH=/Users/moredhel/.boot2docker/certs/boot2docker-vm
export DOCKER_TLS_VERIFY=1
export PIP_REQUIRE_VIRTUALENV=true

PERL_MB_OPT="--install_base \"/Users/moredhel/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/moredhel/perl5"; export PERL_MM_OPT;
export PERL5LIB=/Users/moredhel/perl5/lib/perl5/

unsetopt correct_all


source $HOME/.zshrc_local
