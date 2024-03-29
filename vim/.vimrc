set nocompatible
filetype off

" Load our custom defined Vim functions
source ~/.vim/functions.vim

let mapleader="\<space>"

if empty(glob('~/.vim/autoload/plug.vim'))
  echo "Installing vim-plug"
  silent !mkdir -p ~/.vim/autoload
  silent !curl -fLo ~/.vim/autoload/plug.vim https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.vim/plugged')

" Additional bundles
Plug 'Raimondi/delimitMate'
Plug 'altercation/vim-colors-solarized'
Plug 'ap/vim-you-keep-using-that-word'
Plug 'bling/vim-airline'
Plug 'danro/rename.vim'
Plug 'darfink/starsearch.vim'
Plug 'darfink/vim-plist'
Plug 'editorconfig/editorconfig-vim'
Plug 'elzr/vim-json', { 'for': 'json' }
Plug 'groenewege/vim-less', { 'for': 'less' }
Plug 'hail2u/vim-css3-syntax', { 'for': 'css' }
Plug 'junegunn/vim-easy-align'
Plug 'justinmk/vim-gtfo'
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
Plug 'maxbrunsfeld/vim-emacs-bindings'
Plug 'ntpeters/vim-better-whitespace'
Plug 'othree/html5.vim'
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'terryma/vim-multiple-cursors'
Plug 'tommcdo/vim-exchange'
Plug 'tpope/vim-characterize'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-vinegar'
Plug 'valloric/MatchTagAlways'
Plug 'wellle/targets.vim'

call plug#end()

" Enable file type specifics
filetype plugin indent on

" Custom Vim settings
set autoindent
set backspace=indent,eol,start
set backup
set backupskip=/tmp/*,/private/tmp/*
set complete-=i
set concealcursor=i
set conceallevel=2
set completeopt=longest,menu
set cryptmethod=blowfish
set cursorline
set display+=lastline
set encoding=utf-8 nobomb
set expandtab
set exrc
set fileformats+=mac
set guioptions+=c
set guioptions-=L
set guioptions-=T
set guioptions-=r
set hidden
set history=1000
set hlsearch
set ignorecase
set incsearch
set laststatus=2
set lazyredraw
set listchars=tab:▸\ ,trail:·,eol:¬,nbsp:_
set modeline
set modelines=5
set mouse=nicr
set noerrorbells
set nostartofline
set nowrap
set nrformats-=octal
set number
set pumheight=15
set relativenumber
set ruler
set scrolloff=7
set secure
set sessionoptions-=options
set shiftround
set shiftwidth=2
set shortmess=atI
set showbreak=⇶
set showcmd
set sidescroll=1
set sidescrolloff=30
set smartcase
set smarttab
set splitbelow
set softtabstop=2
set synmaxcol=500
set tabpagemax=50
set tabstop=2
set tags+=.tags
set ttimeout
set ttimeoutlen=100
set ttyfast
set undofile
set undolevels=1000
set undoreload=10000
set updatetime=500
set visualbell
set wildignore=*.o,*.obj,*.bak,*.exe,*.pyc,*~,*.Cache,*DS_Store*,.git
set wildmenu
set writebackup

" Use English for spell checking, but don't spell check by default
if version >= 700
  set spelllang=en spell
  set nospell
endif

" Vim does not support non-POSIX shells (such as fish)
if &shell =~# 'fish$'
  set shell=bash
endif

if !empty(&viminfo)
  set viminfo^=!
endif

" Use par to reflow text
" see: http://vimcasts.org/episodes/formatting-text-with-par/
" Using `gw` will reflow with Vim's built-in algorithm.
if executable('par')
  set formatprg="par -h -w78 -B=.,\?_A_a "
endif

" Improve syntax performance
syntax enable
syntax sync minlines=256

if has('gui_running')
  set background=light

  if GetRunningOS() == 'osx'
    set guifont=Inconsolata\ for\ Powerline:h15
  else
    set guifont=Inconsolata\ for\ Powerline\ Medium\ 12
  endif
else
  set background=dark
endif

try
  let g:solarized_termcolors=16
  colorscheme solarized
catch /^Vim\%((\a\+)\)\=:E185/
endtry

" We want to use a more subtle highlight color
highlight ColorColumn ctermbg=Black guibg=#EDE7D5
let &colorcolumn=join(range(81,999),",")

highlight Normal ctermbg=NONE guibg=NONE

" Extend % operator to HTML tags
runtime macros/matchit.vim

" Setup Vim directories (backup, view, swap, undo)
call InitializeDirectories()

" Return to last edit position when opening files
" Remember last location in file, but not for commit messages.
autocmd BufReadPost *
\ if &filetype !~ '^git\c' && line("'\"") > 0 && line("'\"") <= line("$") |
\   exe "normal! g`\"" |
\ endif

" Prevent clipboard from being cleared by vim
autocmd VimLeave * call system("xsel -ib", getreg('+'))

" Resize splits when the window is resized
autocmd VimResized * exe "normal! \<c-w>="

" File type specifics
autocmd FileType css,scss,less setlocal iskeyword+=-

" We want enter to work inside quickfix windows
autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>

" Enable spell checking for git commit messages
autocmd FileType gitcommit setlocal spell

" Make netrw windows have a fixed width
autocmd FileType netrw setlocal winfixwidth

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=tern#Complete
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Enable syntax for Vue files
autocmd BufNewFile,BufRead *.vue set ft=html

" Change cursor in insert mode
let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"

" Indent all tags in HTML files
let g:html_indent_inctags = "html,body,head,tbody"

" Will allow you to use :w!! to write to a file using sudo if you forgot to sudo
" vim file (it will prompt for sudo password when writing)
" http://stackoverflow.com/questions/95072/what-are-your-favorite-vim-tricks/96492#96492
cmap w!! %!sudo tee > /dev/null %

" Syntastic
let g:syntastic_error_symbol = '✗'
let g:syntastic_warning_symbol = '⚠'
let g:syntastic_full_redraws = 1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_javascript_checkers = ['jshint']
let g:syntastic_python_checkers = ['flake8', 'pep8']
let g:syntastic_json_checkers = ['jsonlint']
let g:syntastic_scss_checkers = ['sass']
let g:syntastic_zsh_checkers = ['shellcheck']
let g:syntastic_sh_checkers = ['shellcheck']
let g:syntastic_cs_checkers = ['syntax', 'semantic', 'issues']
let g:syntastic_php_checkers = ['php']

" Multiple cursors
let g:multi_cursor_use_default_mapping = 0
let g:multi_cursor_start_word_key = 'gb'
let g:multi_cursor_next_key = 'gb'
let g:multi_cursor_quit_key = '<Esc>'

" Better Whitespace
let g:better_whitespace_filetypes_blacklist = ['unite', 'vim-plug']

" Edit vimrc
noremap <silent> <leader>ve :edit $MYVIMRC<cr>
noremap <silent> <leader>vr :source $MYVIMRC<cr>

" EasyAlign
vmap <enter> <plug>(EasyAlign)

" Plist
let g:plist_display_format = 'xml'
let g:plist_save_format = ''

" Airline
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

" Netrw
let g:netrw_altv = 1
let g:netrw_browse_split = 4
let g:netrw_liststyle = 3
let g:netrw_list_hide = &wildignore
let g:netrw_winsize = 20

" Leader bindings
map <leader>q :bufdo bdelete<cr>
map <leader>t :Explore<cr>
map <leader>c :CtrlPTag<cr>

" Code folding options
nmap <leader>f0 :set foldlevel=0<CR>
nmap <leader>f1 :set foldlevel=1<CR>
nmap <leader>f2 :set foldlevel=2<CR>
nmap <leader>f3 :set foldlevel=3<CR>
nmap <leader>f4 :set foldlevel=4<CR>
nmap <leader>f5 :set foldlevel=5<CR>
nmap <leader>f6 :set foldlevel=6<CR>
nmap <leader>f7 :set foldlevel=7<CR>
nmap <leader>f8 :set foldlevel=8<CR>
nmap <leader>f9 :set foldlevel=9<CR>

" Mapping Q (shift-q) to last used buffer
" This binding also feels intuitive if you've ever played a game like CS
" Where last weapon is on Q
nmap Q :b#<CR>

" Faster save (and like other editors)
nnoremap <leader>wa :wa<cr>
nnoremap <leader>w :w<cr>

" Walk through wrapped lines
nnoremap <expr> k (v:count == 0 ? 'gk' : 'k')
nnoremap <expr> j (v:count == 0 ? 'gj' : 'j')

" Make Y consistent with C and D.  See :help Y.
nnoremap Y y$

" This unsets the 'last search pattern' register by hitting return
nnoremap <CR> :noh<CR>

" This disables empty line deletes to set the register
nnoremap <expr> dd match(getline('.'), '^\s*$') == -1 ? 'dd' : '"_dd'

cnoremap <C-A>  <Home>
cnoremap <C-E>  <End>

" What does this actually do?
inoremap <C-U> <C-G>u<C-U>

" Enable shifting between buffers
nnoremap ä :bnext<CR>
nnoremap ö :bprevious<CR>
nmap <C-S-Tab> :bprevious<CR>
nmap <C-Tab> :bnext<CR>
