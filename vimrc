"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Maintainer: Gabriel Chavez 
" Version: Tue Apr  3 23:01:04 CDT 2012

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Use Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" Sets how many lines of history VIM has to remember
set history=700

"Change the mapleader from '\' to ','
let mapleader = ","


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Pathogen
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call pathogen#infect()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Vundle
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
filetype off                   " required!
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'

" Repos from github
Bundle  'kien/ctrlp.vim'
Bundle  'scrooloose/syntastic'
Bundle  'scrooloose/nerdtree'
Bundle  'scrooloose/nerdcommenter'
Bundle  'tpope/vim-fugitive'
Bundle  'tpope/vim-surround'
Bundle  'sjl/gundo.vim'
Bundle  'fholgado/minibufexpl.vim'
Bundle  'ashwin/vim-powerline'
Bundle  'wlangstroth/vim-racket'
Bundle  'plasticboy/vim-markdown'
Bundle  'godlygeek/tabular'
Bundle  'tpope/vim-rails'



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Ctrl-P
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set runtimepath^=~/.vim/bundle/ctrlp.vim
let g:ctrlp_map = '<Leader>t'


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable filetype plugin
if has("autocmd")
    filetype plugin indent on
endif


" Set to auto read when a file is changed from the outside
set autoread

" Set to the current directory when a file is opened
" Seems to be a little incompatible with command+t 
"set autochdir

" Set the wild menu
set wildmenu

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM user interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set 7 lines to the curors - when moving vertical..
set so=7

set ruler "Always show current position

"set cmdheight=2 "The commandbar height

set hid "Change buffer - without saving

set ignorecase "Ignore case when searching
set smartcase

set hlsearch "Highlight search things

set incsearch "Make search act like search in modern browsers
set nolazyredraw "Don't redraw while executing macros 

nmap <leader>q :nohlsearch<CR>

set magic "Set magic on, for regular expressions

set showmatch "Show matching bracets when text indicator is over them
set mat=2 "How many tenths of a second to blink

" No sound on errors
set noerrorbells
set novisualbell
set tm=500

"A red column indicating the 85 width limit
set colorcolumn=85

"Display the status bar always
set laststatus=2

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
syntax enable "Enable syntax hl


" Solarized colorscheme. It has two modes: light and dark.
"set background=dark 
"colorscheme solarized
colorscheme molokai

"Mustang colorscheme
"colorscheme mustang

"line numbers. I guess
"set nu

"set relative line number. Really usefull for vim commands!
set relativenumber

set encoding=utf8


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Good use of Vim and modifying normal behavior
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>
nnoremap j  gj
nnoremap k  gk

" I don't need help. No this way.
inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

"Editing my vimrc
nnoremap <leader>ev <C-w><C-v><C-l>:e $MYVIMRC<cr>

"No more reaching for the Esc key 
imap jj <Esc>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set expandtab
set shiftwidth=4
set tabstop=4
set smarttab


set lbr "Line break
set tw=500 "Text width

set ai "Auto indent
set si "Smart indet
set wrap "Wrap lines

set nopaste "Disables annoying vim deafult behaviour when pasting

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Gundo plugin
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <leader>g :GundoToggle<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Spell checking
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Toggle spell checking on and off with `,s` for Spanish
nmap <silent> <leader>s :set spl=es spell!<CR>
" Toggle english spell checking
nmap <silent> <leader>n :set spl=en spell!<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Window Movement
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" easier navigation between split windows
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Easy yanking and pasting from system clipboard
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
noremap <leader>y "+y
noremap <leader>p "+p

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => vim-latex suite
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set grepprg=grep\ -nH\ $*
let g:tex_flavor = "latex"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => ctags
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <f12> :!ctags -R<cr>
nmap <F8> :TlistToggle <CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Emacs-style momevement for the vim command line
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
cnoremap <C-a>  <Home>
cnoremap <C-b>  <Left>
cnoremap <C-f>  <Right>
cnoremap <C-d>  <Delete>
cnoremap <M-b>  <S-Left>
cnoremap <M-f>  <S-Right>
cnoremap <M-d>  <S-right><Delete>
cnoremap <Esc>b <S-Left>
cnoremap <Esc>f <S-Right>
cnoremap <Esc>d <S-right><Delete>
cnoremap <C-g>  <C-c>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => NERDTree bindings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nmap <Leader>e :NERDTreeToggle<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => MBE settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <Leader>mbt :MBEToggle<cr>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => vim-markdown
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:vim_markdown_folding_disabled=1
