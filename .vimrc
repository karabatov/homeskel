" Don't be compatible with vi
set nocompatible

" Vundle
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim/
let path='~/.vim/bundle'
call vundle#begin(path)

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" YouCompleteMe
" Plugin 'Valloric/YouCompleteMe'

" NERDTree
" Plugin 'scrooloose/nerdtree'

" NERDCommenter
" Plugin 'scrooloose/nerdcommenter'

" Commentary
Plugin 'tpope/vim-commentary'

" Ack
Plugin 'mileszs/ack.vim'

" Surrond
Plugin 'tpope/vim-surround'

" Repeat
Plugin 'tpope/vim-repeat'

" Fugitive
Plugin 'tpope/vim-fugitive'

" Command-T
" Plugin 'wincent/command-t'

" Tell Vundle to download & import the clang complete plugin
" Plugin 'Rip-Rip/clang_complete'

" Completion
" Plugin 'guns/ultisnips'

" iOS
" Plugin 'b4winckler/vim-objc'
" Plugin 'eraserhd/vim-ios.git'
" Plugin 'kentaroi/cocoa.vim'

" Dash
" Plugin 'rizzatti/dash.vim'

" Syntastic
" Plugin 'scrooloose/syntastic'

" DelimitMate
" Plugin 'Raimondi/delimitMate'

" ListToggle
Plugin 'Valloric/ListToggle'

" SuperTab
" Plugin 'ervandew/supertab'

" Swift
Plugin 'toyamarinyon/vim-swift'
" Plugin 'kballard/vim-swift-extra'

" Syntastic
Plugin 'scrooloose/syntastic'

" Easytags
" Plugin 'xolox/vim-misc'
" Plugin 'xolox/vim-easytags'

" Ctrlp
Plugin 'kien/ctrlp.vim'
Plugin 'tacahiroy/ctrlp-funky'

" Colorschemes
Plugin 'flazz/vim-colorschemes'

" All of your Plugins must be added before the following line
call vundle#end()            " required
" filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
nnoremap <leader>pi :PluginInstall!<CR>
nnoremap <leader>pc :PluginClean!<CR>

set backupdir=~/.vim/backup//
set directory=~/.vim/swap//
set undodir=~/.vim/undo//


" Security?
set modelines=0

" Reload
set autoread

" Syntax highlighting
syntax on

" Don't apply autoindent to paste
set pastetoggle=<F2>

" Line numbers
set number

" Status line
set statusline=%m%t%r%h%w\ [%{&ff}]\ %{fugitive#statusline()}\ %#warningmsg#\ %{SyntasticStatuslineFlag()}%*%=%l,%v\ %p%%
" set statusline=%m%t%r%h%w\ [%{&ff}]%=%l,%v\ %p%%\

" No idea!
set laststatus=2

" Auto indent as you go
set autoindent

" Tab and shift width
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

" Color scheme
colorscheme zenburn

" MacVim font
set guifont=Hack:h14

" Highlight the current line
set cursorline

" GUI
set guioptions=egmrt

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

" GUI size. Probably should just go fullscreen.
"set lines=60 columns=236

" No idea
set scrolloff=3

" Show current mode in status line 
set showmode

" No idea
set hidden

" No idea
set wildmenu
set wildmode=list:longest

" Bell visually
set visualbell

" No idea
set ttyfast

" No idea WTF
set ruler

set splitright " Split right not up

" Backspace priority
set backspace=indent,eol,start

" Map leader to comma
let mapleader = ","

" Normal regex and search
nnoremap / /\v
vnoremap / /\v
set ignorecase
set smartcase
set gdefault

" Search
set incsearch
" set showmatch
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

" Hey ; is quicker than :
" nnoremap ; :

" Save
nnoremap <C-s> :w<CR>
inoremap <C-s> <ESC>:w<CR>

" Save on lose focus
au FocusLost * :wa

" Strip all trailing whitespace in the current file
nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>

" Ack
nnoremap <leader>a :Ack 

" Select text that was just pasted
nnoremap <leader>v V`]

" Open vimrc in a vertical split
nnoremap <leader>ev <C-w><C-v><C-l>:e $MYVIMRC<cr>
nnoremap <leader>rv :so ~/.vimrc<CR>

" Auto reload .vimrc on save
augroup myvimrc
    au!
    au BufWritePost .vimrc,_vimrc,vimrc,.gvimrc,_gvimrc,gvimrc so $MYVIMRC | if has('gui_running') | so $MYVIMRC | endif
augroup END

" jj goes back to normal mode
inoremap jj <ESC>

" Open a new vertical split
nnoremap <leader>w <C-w>v<C-w>l

" Move across splits
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

set matchtime=1 " Default 5 is too slow (in 10ms)
set mouse=a " Enable mouse usage (all modes)
set showcmd " Show (partial) command in status line
set synmaxcol=400 " Helps prevent vim from choking on long lines

" File changes
set autoread
au CursorHold,BufWinEnter * checktime

" Relative line numbers
set relativenumber

" NERDTree
" nnoremap <silent> <tab><tab> :NERDTreeToggle<CR>
" nnoremap <silent> <tab>f :NERDTreeFind<CR>
" let NERDTreeShowBookmarks=1
" let NERDTreeCaseSensitiveSort=1
" let NERDTreeIgnore=['\.pyc$', '\~$', '\.git$', '\.gypd$'] " Toggle filtering via default f keybinding

" Commentary
nmap <silent> <C-\> <Plug>CommentaryLine
au Filetype c,cpp,objc,objcpp,html set commentstring=//%s

" Filetype
" au BufRead,BufNewFile *.h,*.m set filetype=objc
au BufRead,BufNewFile Podfile set filetype=ruby

" YouCompleteMe (YCM)
" au Filetype python,c,cpp,objc,objcpp nnoremap <silent> <buffer> <leader>jd :YcmCompleter GoTo<CR>
" au Filetype python,c,cpp,objc,objcpp nnoremap <silent> <buffer> <F6> :YcmCompleter ClearCompilationFlagCache<CR>
" au Filetype python,c,cpp,objc,objcpp nnoremap <silent> <buffer> <F5> :YcmForceCompileAndDiagnostics<CR>
" let g:ycm_confirm_extra_conf = 0
" let g:ycm_always_populate_location_list = 1
" let g:ycm_enable_diagnostic_signs = 0
" let g:ycm_key_list_select_completion = ['<TAB>', '<Down>', '<C-j>']
" let g:ycm_key_list_previous_completion = ['<S-TAB>', '<Up>', '<C-k>']
" let g:ycm_filetype_specific_completion_to_disable = { 'java': 1 }
" let g:ycm_seed_identifiers_with_syntax = 1
" let g:ycm_autoclose_preview_window_after_completion = 1
" let g:ycm_key_detailed_diagnostics = '<leader>yd'
" let g:ycm_autoclose_preview_window_after_insertion = 1

" Dash
" nmap <silent> <leader>d <Plug>DashSearch

" iOS
" Jump to related file
" nmap <silent> <leader>A :A<CR>

" clang_complete settings
" let g:clang_library_path="/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib"
" let g:clang_auto_user_options="compile_commands.json, path, .clang_complete"
" let g:clang_complete_auto = 1
" let g:clang_periodic_quickfix = 1
" let g:clang_close_preview = 1
" For Objective-C, this needs to be active, otherwise multi-parameter methods won't be completed correctly
" let g:clang_snippets = 1
" let g:clang_conceal_snippets=1
" let g:clang_auto_select = 1
" Snipmate does not work anymore, ultisnips is the recommended plugin
" let g:clang_snippets_engine = 'clang_complete'
" show diagnostics
" nnoremap <Leader>q :call g:ClangUpdateQuickFix()<CR>
" let g:clang_complete_copen = 1
" nnoremap <leader>c :cc<cr>

" Limit popup menu height
" set pumheight=15

" SuperTab option for context aware completion
" let g:SuperTabDefaultCompletionType = "context"

" Objective-C related tasks bindings
" nmap <leader>x :!rake vimbuild<CR>
" nmap <leader>r :!rake simulator<CR>
" nmap <leader>ll :!rake debug<CR>
" nmap <leader>cl :!rake clang_db<CR>

" Don't highlight ObjC block brace as error
let c_no_curly_error = 1

" Open framework headers
set includeexpr=1

" Run ctags in project root directory
" nmap <leader>ct :!ctags -R -f ./tags .<CR>
" set tags=tags;/

" Easytags
" nmap <leader>ut :UpdateTags<CR>
" nmap <leader>ht :HighlightTags<CR>
" let g:easytags_opts = ['--langmap=ObjectiveC:.m.h']
" let g:easytags_async=1
" let g:easytags_events = ['BufWritePost']
" let g:easytags_include_members=1

" ListToggle
let g:lt_location_list_toggle_map = '<leader>l'
let g:lt_quickfix_list_toggle_map = '<leader>q'
let g:lt_height=10

" CtrlP
let g:ctrlp_map = '<C-p>'
let g:ctrlp_extensions = ['funky']
let g:ctrlp_show_hidden = 1
nnoremap <leader>fu :CtrlPFunky<CR>
" nnoremap <leader>fi :ListMethods<CR>

" DelimitMate
" let g:delimitMate_expand_cr = 1

" au Filetype swift set makeprg=xcrun\ swiftc\ -parse\ %
" au Filetype swift set errorformat=%E%f:%l:%c: error: %m,%W%f:%l:%c: warning: %m,%Z%\s%#^~%#,%-G%.%#

" Syntastic
let g:syntastic_swift_checkers = ['swiftc']
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" Russian
:set langmap=ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯ;ABCDEFGHIJKLMNOPQRSTUVWXYZ,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz,[!"№%:,.;()_+[]\;',./{}||:"<>?;~!@#$%^&*()_+[]\;',./{}|:"<>?
