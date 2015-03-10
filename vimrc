" Maintainer: Gabriel Chavez 

" Use Vim settings, rather than Vi settings (much better!).
set nocompatible

" Sets how many lines of history VIM has to remember
set history=700

"Change the mapleader from '\' to ','
let mapleader = ","

filetype off                   " required!
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle
Bundle 'gmarik/Vundle.vim'
"
" Repos from github
Plugin 'tpope/vim-fugitive'
Plugin 'scrooloose/nerdtree'
Plugin 'tpope/vim-surround'
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'bling/vim-airline'
Plugin 'tpope/vim-markdown'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'majutsushi/tagbar'
Plugin 'Raimondi/delimitMate'

" Color schemes!
Plugin 'tomasr/molokai'

call vundle#end()

" Ctrl-P
set runtimepath^=~/.vim/bundle/ctrlp.vim
let g:ctrlp_map = '<Leader>e'


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

nmap <leader>q :nohlsearch<CR>

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

" Solarized colorscheme. It has two modes: light and dark.
"set background=dark 
"colorscheme solarized
colorscheme molokai

"Mustang colorscheme
"colorscheme mustang

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

"Editing my vimrc
nnoremap <leader>ev <C-w><C-v><C-l>:e $MYVIMRC<cr>

"No more reaching for the Esc key 
imap jj <Esc>

" Text, tab and indent related
set expandtab
set shiftwidth=4
set tabstop=4
set smarttab

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
nmap <silent> <leader>s :set spl=es spell!<CR>
" Toggle english spell checking
nmap <silent> <leader>n :set spl=en spell!<CR>

" Window Movement, easier navigation between split windows
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

" Easy yanking and pasting from system clipboard
noremap <leader>y "+y
noremap <leader>p "+p

" vim-latex suite
" set grepprg=grep\ -nH\ $*
" let g:tex_flavor = "latex"

" ctags
nnoremap <f12> :!ctags -R<cr>
nmap <leader>b :TagbarToggle <CR>

" Emacs-style momevement for the vim command line
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

" => vim-markdown
" let g:vim_markdown_folding_disabled=1

" Custom Autocmd's
 augroup lang
 autocmd FileType racket,ruby,haml,eruby,scss,yaml,html,javascript,cucumber set ai sw=2 sts=2 et
 autocmd FileType python set sw=4 sts=4 et
 augroup end

" Ultisnips
" Trigger configuration. 
" Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<C-b>"
let g:UltiSnipsJumpBackwardTrigger="<C-z>"

" We use NERDTree now
nmap <Leader>t :NERDTreeToggle<cr>

" vim-go settings
au FileType go nmap <leader>r <Plug>(go-run)
