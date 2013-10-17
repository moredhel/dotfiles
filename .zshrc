CASE_SENSITIVE="true"
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="test"
ncmpcppShow() { BUFFER="ncmpcpp"; zle accept-line; }
zle -N ncmpcppShow
bindkey '^[\' ncmpcppShow
alias _="sudo"
alias aoeu="kinit s1249759@INF.ED.AC.UK"
alias snth="ncmpcpp"
alias cdp="cd ~/documents/lecture_notes"
alias t="task"
source $ZSH/oh-my-zsh.sh
[ -n "$XTERM_VERSION" ] && transset-df -a >/dev/null
export PATH=$PATH:~/.cabal/bin:~/.xmonad/bin
