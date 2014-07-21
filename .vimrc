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

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
Plugin 'ervandew/supertab'
Plugin 'tpope/vim-fugitive'
Bundle 'Valloric/YouCompleteMe'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required


colorscheme desert
set background=dark
set encoding=utf-8
set scrolloff=5
autocmd FileType text set spell
au BufRead,BufNewFile *.coffee set filetype=ruby
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
