set enc=utf-8
set nocompatible
filetype off                  " required

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" Languages
" Plugin 'fatih/vim-go'
Plugin 'adimit/prolog.vim'
" Plugin 'koron/minimap-vim'
Plugin 'jelera/vim-javascript-syntax'
Plugin 'pangloss/vim-javascript'
Plugin 'derekwyatt/vim-scala'
Plugin 'vim-airline/vim-airline-themes'
" Error
Plugin 'scrooloose/syntastic'
" Utils
Plugin 'groenewege/vim-less'
Plugin 'scrooloose/nerdtree'
Plugin 'bling/vim-airline'
Plugin 'Shougo/vimproc.vim'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'severin-lemaignan/vim-minimap'
Plugin 'Shougo/vimshell'
Plugin 'easymotion/vim-easymotion'
Plugin 'airblade/vim-gitgutter'
Plugin 'rking/ag.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'majutsushi/tagbar'
Plugin 'thinca/vim-quickrun'
Plugin 'fisadev/vim-ctrlp-cmdpalette'
Plugin 'Shougo/neocomplete.vim'
Plugin 'tpope/vim-commentary'
Plugin 'bkad/CamelCaseMotion'
Plugin 'sjl/gundo.vim'
Plugin 'jiangmiao/auto-pairs'
" Plugin 'vim-scripts/taglist.vim'
" Plugin 'terryma/vim-multiple-cursors'
Plugin 'altercation/vim-colors-solarized'
Plugin 'morhetz/gruvbox'
" Plugin 'chriskempson/base16-vim'
call vundle#end()

" ================
" GENERAL SETTINGS
" ================

filetype plugin indent on
syntax on

" TAB SETTINGS
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

set encoding=utf-8
set scrolloff=3
set autoindent
set showcmd
set wildmenu
" set wildmode=list:longest
set cursorline
set ttyfast
set laststatus=2
set backspace=2
set clipboard=unnamed
set mouse+=a
if has('persistent_undo')
    set undofile
    set undodir=/tmp
endif

" SEARCHING / MOVING
let mapleader = ","
set ignorecase
set smartcase
" set gdefault " replace text globally
set incsearch
set showmatch
nnoremap <tab> %
vnoremap <tab> %

" LONG LINE HANDLING
set wrap linebreak nolist
set textwidth=79
set formatoptions=qnr1
" set colorcolumn=81
set foldmethod=indent
set foldnestmax=10
set nofoldenable
set foldlevel=2

" CONFUSING REMAPPING
autocmd InsertEnter * :let @/=""
autocmd InsertLeave * :let @/=""

set shell=/bin/bash

" ===================
" APPEARANCE SETTINGS
" ===================
" Colorscheme
colorscheme gruvbox
set background=dark
" Lines
set nu
if exists('+relativenumber')
    set relativenumber
endif

set list listchars=tab:\|\ ,trail:~,extends:>,precedes:<
set scrolloff=5 "keep at least 5 lines above/below

" Search settings
set hlsearch " highlight all matches of a search
set incsearch " searches as you type


" =================
" KEYBOARD SETTINGS
" =================

" Specific files
nnoremap <leader>r :so ~/.vimrc<CR>

" Easier Commands
nnoremap ; :
inoremap jj <ESC>
nnoremap <leader>v <C-w>v<C-w>l
nnoremap <leader>s <C-w>s<C-w>l
nnoremap <leader>q <C-w><C-q>

" Sane movements
nnoremap j gj
nnoremap k gk

" Window switching
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h

" shortcuts for buffer manipulation
nnoremap <silent> [b :bprevious<CR>
nnoremap <silent> ]b :bnext<CR>
nnoremap <silent> ]B :bfirst<CR>
nnoremap <silent> ]B :blast<CR>

set pastetoggle=<F2>
noremap p <F2>p<F2>

" Turns off highlight using this key map
map <C-c> :noh<cr>

" ===============
" PLUGIN SETTINGS
" ===============

" NERDTree config
map <C-n> :NERDTreeToggle<CR>
let NERDTreeChDirMode=2
let NERDTreeIgnore=['\.vim$', '\~$', '\.pyc$', '\.swp$', '__pycache__$']
let NERDTreeSortOrder=['^__\.py$', '\/$', '*', '\.swp$',  '\~$']
let NERDTreeShowBookmarks=1

" Gundo
" nnoremap <F5> :GundoToggle<CR>

" ctrlP settings
set runtimepath^=~/.vim/bundle/ctrlp.vim
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
set wildignore+=*.pyc
set wildignore+=*/node_modules/*
set wildignore+=Library/*
set wildignore+=Downloads/*
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_working_path_mode = 'r'
let g:ctrlp_extensions = ['line']
noremap <C-U> :CtrlPCmdPalette<CR>


" Airline
let g:airline_theme = 'gruvbox'
let g:airline_detect_modified = 1
let g:airline_detect_paste = 1
let g:airline_powerline_fonts=1

let g:airline#extensions#tagbar#enabled = 1
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#syntastic#enabled = 1
let g:airline#extensions#tmuxline#enabled = 1
let g:airline#extensions#whitespace#enabled = 1
let g:airline#extensions#whitespace#checks = [ 'indent', 'trailing']
let g:airline_mode_map = {
            \ '__' : '-',
            \ 'n'  : 'N',
            \ 'i'  : 'I',
            \ 'R'  : 'R',
            \ 'c'  : 'C',
            \ 'v'  : 'VL',
            \ 'V'  : 'VB',
            \ '' : 'Vsl',
            \ 's'  : 'LSbs',
            \ }

let g:neocomplete#enable_at_startup = 1

" MacVim settings
set guifont=Fira\ Code:h18
" set guifont=Sauce\ Code\ Powerline\ Light:h18
if has("gui_macvim")
    let macvim_hig_shift_movement = 1
    noremap <D-P> :CtrlPCmdPalette<CR>
    noremap <D-/> :Commentary<CR>
endif

