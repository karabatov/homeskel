" Language
set langmenu=none
language C

" Plugins
call plug#begin('~/.config/nvim/plugged')
Plug 'neomake/neomake'
Plug 'freitass/todo.txt-vim'
Plug 'plasticboy/vim-markdown'
Plug 'junegunn/goyo.vim'
call plug#end()

" Syntax highlighting
syntax on

" Don't apply autoindent to paste
set pastetoggle=<F2>

" Line numbers
set number

" Auto indent as you go
set autoindent

" Tab and shift width
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

" Color scheme
" colorscheme zenburn

" Highlight the current line
" set cursorline

" Default file encodings
set fileencodings=utf-8,cp1251,koi8-r,cp866
set encoding=utf-8
set termencoding=utf-8

" displaying tab characters and trailing spaces
" with special characters \u2592\u2591 and \u2593
set lcs=tab:▒░,trail:▓
set list

" Line movement: move by screen lines, not file lines
noremap j gj
noremap k gk
noremap gj j
noremap gk k

" Show current mode in status line
set showmode

" Bell visually
set visualbell

set splitright " Split right not up

" Backspace priority
set backspace=indent,eol,start

" Map leader to comma
let mapleader = ","

" Normal regex and search
set ignorecase
set smartcase
set gdefault

" Search
set incsearch
set showmatch
set hlsearch

" Type ,<space> to get rid of search highlighting
nnoremap <leader><space> :noh<cr>

" Jump between parentheses with TAB, not %
nnoremap <tab> %
vnoremap <tab> %

" Line wrapping
set wrap
set textwidth=79
set formatoptions=qrn1

" HARDCORE MODE
"nnoremap <up> <nop>
"nnoremap <down> <nop>
"nnoremap <left> <nop>
"nnoremap <right> <nop>
"inoremap <up> <nop>
"inoremap <down> <nop>
"inoremap <left> <nop>
"inoremap <right> <nop>

" Fat fingers
inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

" Copy and paste
nnoremap <leader>y "+y
vnoremap <leader>y "+y
nnoremap <leader>p "+p
vnoremap <leader>p "+p

" Save
nnoremap <C-s> :w<CR>
inoremap <C-s> <ESC>:w<CR>

" Save on lose focus
au FocusLost * :wa

" Cycle buffers
nnoremap <C-Tab> :bn<CR>
nnoremap <C-S-Tab> :bp<CR>

" Strip all trailing whitespace in the current file
nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>

" Select text that was just pasted
nnoremap <leader>v V`]

" Open nvimrc in a vertical split
nnoremap <leader>ev <C-w><C-v><C-l>:e $MYVIMRC<cr>
nnoremap <leader>rv :so $MYVIMRC<CR>

" Auto reload nvimrc on save
 augroup myvimrc
    au!
    au BufWritePost $MYVIMRC so $MYVIMRC
 augroup END

" Open a new vertical split
nnoremap <leader>w <C-w>v<C-w>l

" Move across splits
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

set mouse=a " Enable mouse usage (all modes)
set showcmd " Show (partial) command in status line

" File changes
set autoread
au CursorHold,BufWinEnter * checktime

" Relative line numbers
set relativenumber

" C filetype
augroup project
    autocmd!
    autocmd BufRead,BufNewFile *.h,*.c set filetype=c
".doxygen
augroup END

" Neomake
autocmd! BufWritePost * Neomake

" make run
nnoremap <leader>mr :!make run<CR>

" Change dir to current file
autocmd BufEnter * silent! lcd %:p:h

" Russian
:set langmap=ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯ;ABCDEFGHIJKLMNOPQRSTUVWXYZ,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz,[!"№%:,.;()_+[]\;',./{}||:"<>?;~!@#$%^&*()_+[]\;',./{}|:"<>?
