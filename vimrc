"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Maintainer: Gabriel Chavez 
" Version: 1.1 - 10/03/11 
" This is for my personal use. It was inspired by amix vimrc
" http://http://amix.dk/vim/vimrc.html
"
" Plugins_installed:
"	> vim-snipmate
"		You can make vim behaves like the Textmate editor
"	> NERDtree
"		File viewer side-pane
"	> vim-rails
"		A plugin to improve the development of Ruby on rails

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

set cmdheight=2 "The commandbar height

set hid "Change buffer - without saving

set ignorecase "Ignore case when searching
set smartcase

set hlsearch "Highlight search things

set incsearch "Make search act like search in modern browsers
set nolazyredraw "Don't redraw while executing macros 

set magic "Set magic on, for regular expressions

set showmatch "Show matching bracets when text indicator is over them
set mat=2 "How many tenths of a second to blink

" No sound on errors
set noerrorbells
set novisualbell
set tm=500


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
syntax enable "Enable syntax hl

" Really good coloscheme. Similar to Textmate's
colorscheme molokai

" Solarized colorscheme. It has two modes: light and dark.
"set background=dark 
"colorscheme solarized

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

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set expandtab
set shiftwidth=4
set tabstop=4
set smarttab

set lbr
set tw=500

set ai "Auto indent
set si "Smart indet
set wrap "Wrap lines

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
set colorcolumn=85

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => vim-latex suite
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set grepprg=grep\ -nH\ $*
let g:tex_flavor = "latex"

