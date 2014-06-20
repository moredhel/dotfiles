set encoding=utf-8
set scrolloff=3
autocmd FileType text set spell
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
