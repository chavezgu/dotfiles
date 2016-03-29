" Maintainer: Gabriel Chavez

" Use Vim settings, rather than Vi settings (much better!).
set nocompatible

" Sets how many lines of history VIM has to remember
set history=700

"Change the mapleader from '\' to ','
let mapleader = "\<Space>"

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

" Set 7 lines to the curors - when moving vertical..
set so=7

" Always show current position
set ruler

" The commandbar height
"set cmdheight=2

" Change buffer - without saving
set hid

" Ignore case when searching
set ignorecase
set smartcase

"Highlight search things
set hlsearch

"Make search act like search in modern browsers
set incsearch
"Don't redraw while executing macros

set nolazyredraw

"Remove highlight
nmap <leader>nh :nohlsearch<CR>

"Easy transition between files.
nnoremap <leader><leader> <c-^>

"Set magic on, for regular expressions
set magic

"Show matching bracets when text indicator is over them
set showmatch

"How many tenths of a second to blink
set mat=2

" No sound on errors
set noerrorbells
set novisualbell
set tm=500

"A red column indicating the 79 width limit
set colorcolumn=80

"Display the status bar always
set laststatus=2
"
"Enable syntax hl
syntax enable

" Set molokai colors
" colorscheme molokai

"line numbers. I guess
set nu

"TOO SLOW! set relative line number. Really usefull for vim commands!
" set relativenumber

set encoding=utf8

" Good use of Vim and modifying normal behavior

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

"No more reaching for the Esc key
imap jj <Esc>

"Line break
set lbr

"Text width
set tw=500

"Auto indent
set ai

"Smart indet
set si

"Wrap lines
set wrap

set nopaste "Disables annoying vim deafult behaviour when pasting

" Spell checking
" Toggle spell checking on and off with `,s` for Spanish
nmap <silent> <leader>ss :set spl=es spell!<CR>
" Toggle english spell checking
nmap <silent> <leader>se :set spl=en spell!<CR>

" Window Movement, easier navigation between split windows
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

" Easy yanking and pasting from system clipboard
noremap <leader>y "+y
noremap <leader>p :set paste<CR>"+p:set paste!<CR>

" Text, tab and indent related
set expandtab
set shiftwidth=8
set tabstop=8
set softtabstop=8
set nosmarttab

" Custom Autocmd's
augroup lang
autocmd FileType racket,ruby,haml,eruby,scss,yaml,html,javascript,cucumber set ai sw=2 sts=2 ts=2 et
autocmd FileType python set sw=4 sts=4 ts=4 et
augroup end


" Quit faster
"noremap <leader>q :q!<CR>

