#oh-my-zsh stuff
CASE_SENSITIVE="true"
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="custom"
plugins=(archlinux git git-flow zsh-syntax-highlighting)
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
export PATH=~/.cabal/bin:~/bin:$PATH:~/.cabal/bin:~/.xmonad/bin
export EDITOR="vim"
export PATH="$HOME/bin:$HOME/.gem/ruby/2.1.0/bin:$HOME/.rbenv/bin:$PATH"
export GPG_TTY=`tty`
unset GREP_OPTIONS

PATH="/home/hamhut/perl5/bin${PATH+:}${PATH}"; export PATH;
PERL5LIB="/home/hamhut/perl5/lib/perl5${PERL5LIB+:}${PERL5LIB}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/hamhut/perl5${PERL_LOCAL_LIB_ROOT+:}${PERL_LOCAL_LIB_ROOT}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/hamhut/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/hamhut/perl5"; export PERL_MM_OPT;
function cd {
    builtin cd $1
    echo `pwd` > ~/.last_location
}
cd `cat ~/.last_location`
