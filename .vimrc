set visualbell
set encoding=utf-8
set scrolloff=3
autocmd FileType text set spell
set foldmethod=syntax
set ts=4
set hls
set smartcase
set number
set relativenumber
set textwidth=80
set history=700
set undolevels=700
set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround
set expandtab
set nobackup
set nowritebackup
set noswapfile
set laststatus=2
autocmd FileType c compiler gcc
" This must be first, because it changes other options as a side effect.
set nocompatible
" allow backspacing over everything in insert mode
set backspace=indent,eol,start

set showcmd		" display incomplete commands
set incsearch		" do incremental searching

map Q gq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

set guioptions-=T
set hlsearch

" Only do this part when compiled with support for autocommands.
filetype plugin indent on
set autoindent		" always set autoindenting on

" moving between panes
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h
