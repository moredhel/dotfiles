CASE_SENSITIVE="true"
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="dieter"
ncmpcppShow() { BUFFER="ncmpcpp"; zle accept-line; }
zle -N ncmpcppShow
bindkey '^[\' ncmpcppShow
alias _="sudo"
alias mail="ssh -t crowfox screen -rd mail"
alias aoeu="kinit s1249759@INF.ED.AC.UK"
alias snth="ncmpcpp"
alias cdp="cd ~/documents/lecture_notes"
alias t="task"
alias g="git"
alias em="emacsclient -t"
alias es="emacsclient"
alias g="git"
source $ZSH/oh-my-zsh.sh
[ -n "$XTERM_VERSION" ] && transset-df -a >/dev/null
export PATH=~/.cabal/bin:~/bin:$PATH:~/.cabal/bin:~/.xmonad/bin
export EDITOR="vim"
export PATH="$HOME/bin:$HOME/.rbenv/bin:$PATH"
export GPG_TTY=`tty`
function cd {
    builtin cd $1
    echo `pwd` > ~/.last_location
}
cd `cat ~/.last_location`
