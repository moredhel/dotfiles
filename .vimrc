set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'mtth/scratch.vim'
Plugin 'burnettk/vim-angular'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
Plugin 'https://github.com/bitc/vim-hdevtools'
Plugin 'ervandew/supertab'
Plugin 'tpope/vim-fugitive'
Plugin 'vim-scripts/textutil.vim'
Plugin 'vim-scripts/Jinja'
" Bundle 'Valloric/YouCompleteMe'
Bundle 'https://github.com/kien/ctrlp.vim'
Bundle 'vim-ruby/vim-ruby'
Bundle 'https://github.com/tpope/vim-eunuch'
Bundle 'https://github.com/tpope/vim-sensible'
Bundle 'https://github.com/tpope/vim-endwise'
Bundle 'https://github.com/tpope/vim-fireplace'
Bundle 'http://github.com/mattn/emmet-vim/'
Bundle 'https://github.com/scrooloose/syntastic'
Bundle 'https://github.com/tpope/vim-dispatch'
Bundle 'https://github.com/bling/vim-airline'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
let g:ycm_collect_identifiers_from_tags_files = 1


" Vim Airline
let g:airline#extensions#tabline#enabled = 1


try
    colorscheme slate
catch
    " catch no colorscheme
endtry
" syntastic
set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
" end-syntastic

set fileformat=unix
set background=dark
set encoding=utf-8
set scrolloff=5
set timeoutlen=100
set nowildmenu
autocmd FileType text set spell
autocmd Filetype html setlocal ts=2 sts=2 sw=2
autocmd Filetype ruby setlocal ts=2 sts=2 sw=2
autocmd Filetype javascript setlocal ts=4 sts=4 sw=4
au BufRead,BufNewFile *.coffee set filetype=ruby
au BufRead,BufNewFile *.jinja set filetype=jinja
runtime /usr/share/vim/vim72/syntax/syntax.vim
syntax on
set ts=2
set sts=2
set sw=2
set hls
set smartcase
set number
if has('relativenumber')
    set relativenumber
endif
set history=700
set undolevels=700
set shiftround
set expandtab
set nobackup
set noswapfile
set laststatus=1
" allow backspacing over everything in insert mode
set backspace=indent,eol,start
set incsearch		" do incremental searching
map Q gq
inoremap <C-U> <C-G>u<C-U>
set autoindent		" always set autoindenting on
" moving between panes
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h
set foldmethod=indent
augroup filetypedetect
    " Mail
    autocmd BufRead,BufNewFile *mutt-*              setfiletype mail
augroup END
set showmatch " Show matching braces.
" Strip the newline from the end of a string
function! Chomp(str)
    return substitute(a:str, '\n$', '', '')
endfunction

" Find a file and pass it to cmd
function! DmenuOpen(cmd)
    let fname = Chomp(system("git ls-files | dmenu -sb '#333' -nf '#aaa' -nb '#000' -l 40 -i -p " . a:cmd))
    if empty(fname)
        return
    endif
    execute a:cmd . " " . fname
endfunction

map <c-t> :call DmenuOpen("tabe")<cr>
map <c-f> :call DmenuOpen("e")<cr>
