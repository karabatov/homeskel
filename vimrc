" Config for Vim with no plugins

" Language
set langmenu=none
language C

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

" Highlight the current line
set cursorline

" Default file encodings
set fileencodings=utf-8,cp1251,koi8-r,cp866
set encoding=utf-8
set termencoding=utf-8

" displaying tab characters and trailing spaces
" with special characters \u2592\u2591 and \u2593
set lcs=tab:▒░,trail:▓
set list

" Show current mode in status line
set showmode

" Bell visually
set visualbell

set splitright " Split right not up

" Backspace priority
set backspace=indent,eol,start

" Map leader to space
let mapleader = " "

" Normal regex and search
set ignorecase
set smartcase
set gdefault

" Search
set incsearch
set showmatch
set hlsearch

" Type <space><space> to get rid of search highlighting
nnoremap <leader><space> :noh<cr>

" Line wrapping
set nolinebreak
set wrap
set textwidth=79
set formatoptions=qrn1

" Copy and paste
nnoremap <leader>y "+y
vnoremap <leader>y "+y
nnoremap <leader>p "+p
vnoremap <leader>p "+p
vmap <C-c> y:call system("xclip -i -selection clipboard", getreg("\""))<CR>:call system("xclip -i", getreg("\""))<CR>

" Save on lose focus
au FocusLost * :wa

" Cycle buffers
nnoremap <C-Tab> :bnext<CR>
nnoremap <C-S-Tab> :bprevious<CR>

" Jump to buffer
nnoremap <Leader>f :set nomore<Bar>:ls<Bar>:set more<CR>:b<Space>

" Strip all trailing whitespace in the current file
nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>

" Select text that was just pasted
nnoremap <leader>v V`]

" Open nvimrc in a vertical split
nnoremap <leader>ev <C-w><C-v><C-l>:e $MYVIMRC<cr>
nnoremap <leader>rv :so $MYVIMRC<CR>

set mouse=a " Enable mouse usage (all modes)
set showcmd " Show (partial) command in status line

" File changes
set autoread
au CursorHold,BufWinEnter * checktime

" Relative line numbers
set relativenumber

" filetype
filetype plugin on

" Change dir to current file
autocmd BufEnter * silent! lcd %:p:h

" Markdown
au! BufRead,BufNewFile *.md set filetype=markdown formatoptions-=t
set conceallevel=2
let g:vim_markdown_autowrite = 1

" Word count
nnoremap <leader>cw :w !wc -w<CR>
