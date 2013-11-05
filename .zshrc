CASE_SENSITIVE="true"
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="hamish"
ncmpcppShow() { BUFFER="ncmpcpp"; zle accept-line; }
zle -N ncmpcppShow
bindkey '^[\' ncmpcppShow
alias _="sudo"
alias aoeu="kinit s1249759@INF.ED.AC.UK"
alias snth="ncmpcpp"
alias t="task"
source $ZSH/oh-my-zsh.sh
[ -n "$XTERM_VERSION" ] && transset-df -a >/dev/null
export PATH=~/bin:$PATH:~/.cabal/bin:~/.xmonad/bin
export EDITOR="vim"
<<<<<<< HEAD

export MARKPATH=$HOME/.marks
function jump { 
    cd -P "$MARKPATH/$1" 2>/dev/null || echo "No such mark: $1"
}
function mark { 
    mkdir -p "$MARKPATH"; ln -s "$(pwd)" "$MARKPATH/$1"
}
function unmark { 
    rm -i "$MARKPATH/$1"
}
function marks {
    ls -l "$MARKPATH" | sed 's/  / /g' | cut -d' ' -f9- | sed 's/ -/\t-/g' && echo
}
=======
task active
echo ""
mu find g:n -f 's f m'
echo ""
>>>>>>> deae690bc38afea4c0b7dc29fecceec9a42f0352
