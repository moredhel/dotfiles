execute pathogen#infect()
"call pathogen()
call pathogen#helptags()
"autocmd VimEnter,BufNewFile,BufReadPost * silent! call HardMode()
syntax on
set visualbell
set ruler		" show the cursor position all the time
set encoding=utf-8
filetype plugin indent on
set scrolloff=3
autocmd FileType text set spell
set foldmethod=syntax
set ts=4
set hls
set smartcase
set number
set textwidth=79
set nowrap
let mapleader = ","
noremap <C-n> :nohl<CR>
vnoremap <C-n> :nohl<CR>
inoremap <C-n> :nohl<CR>
map <Leader>n <esc>:tabprevious<CR>
map <Leader>m <esc>:tabnext<CR>
map <Leader>ev <esc>:e ~/.vimrc<CR>
map <Leader>sv <esc>:source ~/.vimrc<CR>
noremap <C-Z> :update<CR>
vnoremap <C-Z> <C-C>:update<CR>
inoremap <C-Z> <C-O>:update<CR>
set colorcolumn=80
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
map <Leader>g :call RopeGotoDefinition()<CR>
let ropevim_enable_shortcuts = 1
let g:pymode_rope_goto_def_newwin = "vnew"
let g:pymode_rope_extended_complete = 1
let g:pymode_breakpoint = 0
let g:pymode_syntax = 1
let g:pymode_syntax_builtin_objs = 0
let g:pymode_syntax_builtin_funcs = 0
let g:PyLintCWindow = 1
let g:PyLintSigns = 1
let g:PyLintOnWrite = 1
autocmd FileType python compiler pylint
autocmd FileType c compiler gcc
"map <Leader>b Oimport ipdb; ipdb.set_trace() # BREAKPOINT<C-c>

" Better navigating through omnicomplete option list
" " See
" http://stackoverflow.com/questions/2170023/how-to-map-keys-for-popup-menu-in-vim
set completeopt=longest,menuone
function! OmniPopup(action)
    if pumvisible()
        if a:action == 'j'
            return "\<C-N>"
        elseif a:action == 'k'
            return "\<C-P>"
        endif
    endif
    return a:action
endfunction

inoremap <silent><C-j> <C-R>=OmniPopup('j')<CR>
inoremap <silent><C-k> <C-R>=OmniPopup('k')<CR>
set nofoldenable
" An example for a vimrc file.
"
" Maintainer:	Bram Moolenaar <Bram@vim.org>
" Last change:	2011 Apr 15
"
" To use it, copy it to
"     for Unix and OS/2:  ~/.vimrc
"	      for Amiga:  s:.vimrc
"  for MS-DOS and Win32:  $VIM\_vimrc
"	    for OpenVMS:  sys$login:.vimrc

" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
    finish
endif

" Use Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

if has("vms")
    set nobackup		" do not keep a backup file, use versions instead
else
    set backup		" keep a backup file
endif
set history=50		" keep 50 lines of command line history
set showcmd		" display incomplete commands
set incsearch		" do incremental searching

" For Win32 GUI: remove 't' flag from 'guioptions': no tearoff menu entries
" let &guioptions = substitute(&guioptions, "t", "", "g")

" Don't use Ex mode, use Q for formatting
map Q gq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" In many terminal emulators the mouse works just fine, thus enable it.
" if has('mouse')
" set mouse=a
" endif

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
    set guioptions-=T
    syntax on
    set hlsearch
endif

" Only do this part when compiled with support for autocommands.
if has("autocmd")

    " Enable file type detection.
    " Use the default filetype settings, so that mail gets 'tw' set to 72,
    " 'cindent' is on in C files, etc.
    " Also load indent files, to automatically do language-dependent indenting.
    filetype plugin indent on

    " Put these in an autocmd group, so that we can delete them easily.
    augroup vimrcEx
        au!

        " For all text files set 'textwidth' to 78 characters.
        autocmd FileType text setlocal textwidth=78

        " When editing a file, always jump to the last known cursor position.
        " Don't do it when the position is invalid or when inside an event handler
        " (happens when dropping a file on gvim).
        " Also don't do it when the mark is in the first line, that is the default
        " position when opening a file.
        autocmd BufReadPost *
                    \ if line("'\"") > 1 && line("'\"") <= line("$") |
                    \   exe "normal! g`\"" |
                    \ endif

    augroup END

else

    set autoindent		" always set autoindenting on

endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
    command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
                \ | wincmd p | diffthis
endif
nnoremap <leader>h <Esc>:call ToggleHardMode()<CR>
set backupdir=/home/hamhut/documents/vim_backups
map <Leader>c <F9>
map <Leader>x <esc>:AsyncShell /home/hamhut/bin/runc %<CR>
map <Leader>z <esc>:AsyncShell /home/hamhut/bin/runc % "valgrind"<CR>
map <leader>l <esc>:q<CR>
inoremap jj <esc>
noremap <C-C> <esc>A;<esc>:update<CR>o
vnoremap <C-C> <esc>A;<esc>:update<CR>o
inoremap <C-C> <esc>A;<esc>:update<CR>o
noremap <C-Q> <esc>:q<CR>
vnoremap <C-Q> <esc>:q<CR>
inoremap <C-Q> <esc>:q<CR>
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h
noremap <C-V> <esc>:update<CR>A{<CR>}<esc>O
noremap <C-V> <esc>:update<CR>A{<CR>}<esc>O
inoremap <C-V> <esc>:update<CR>A{<CR>}<esc>O
map <leader>q <esc>:!touch spec/features/static_pages_spec.rb<CR>
inoremap <C-Z> <esc>:TagbarToggle<CR>
noremap <C-Z> <esc>:TagbarToggle<CR>
vnoremap <C-Z> <esc>:TagbarToggle<CR>
nnoremap ,cd <esc>:lcd %:p:h<CR>:pwd<CR>
imap <C-H> <C-X><C-O>
colorscheme desert
au BufRead, BufNewFile *.haml set ft=haml
map <F2> :mksession! ~/vim_session <cr> " Quick write session with F2
map <F3> :source ~/vim_session <cr>     " And load session with F3
