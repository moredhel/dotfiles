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
Plugin 'ervandew/supertab'
Plugin 'tpope/vim-fugitive'
Plugin 'vim-scripts/textutil.vim'
Plugin 'vim-scripts/Jinja'
Bundle 'Valloric/YouCompleteMe'
Bundle 'https://github.com/kien/ctrlp.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
let g:ycm_collect_identifiers_from_tags_files = 1

colorscheme desert
set background=dark
set encoding=utf-8
set scrolloff=5
autocmd FileType text set spell
au BufRead,BufNewFile *.coffee set filetype=ruby
au BufRead,BufNewFile *.jinja set filetype=jinja
syntax on
set ts=4
set hls
set smartcase
set number
set relativenumber
set history=700
set undolevels=700
set tabstop=4
set softtabstop=4
set shiftwidth=4
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
set clipboard+=unnamed  " Yanks go on clipboard instead.
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
