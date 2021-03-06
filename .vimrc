set nocompatible
filetype off
syntax enable

let g:haskell_enable_quantification = 1   " highlighting `forall`
let g:haskell_enable_recursivedo = 1      " highlighting `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " highlighting `proc`
let g:haskell_enable_pattern_synonyms = 1 " highlighting `pattern`
let g:haskell_enable_typeroles = 1        " highlighting type roles
let g:haskell_enable_static_pointers = 1  " highlighting `static`
let g:haskell_backpack = 1 " highlighting backpack keywords
let g:haskell_indent_disable = 1

let g:lucius_high_contrast = 1
let g:lucius_style = 'dark'
let g:lucius_no_term_bg = 1

set rtp+=~/.vim/bundle/Vundle.vim
set rtp+=~/.fzf

call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'majutsushi/tagbar'
Plugin 'tpope/vim-surround'
Plugin 'Raimondi/delimitMate'
Plugin 'airblade/vim-gitgutter'
Plugin 'mattn/emmet-vim'
Plugin 'freeo/vim-kalisi'
Plugin 'jonathanfilip/vim-lucius'
Plugin 'scrooloose/nerdcommenter'
Plugin 'mileszs/ack.vim'
Plugin 'google/vim-searchindex'
Plugin 'neovimhaskell/haskell-vim'
Plugin 'junegunn/fzf.vim'
call vundle#end()


"""""""""""""""""""""""""""""""""

set lazyredraw
set ttyfast
set virtualedit=block
set shortmess+=I
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set hidden
set backspace=2
set wildmenu
set wildmode=full
set foldmethod=indent
set foldlevel=99
set hlsearch
set autoread
set showmatch
set matchtime=4
set timeoutlen=1000 ttimeoutlen=0
set updatetime=100
set number
set splitbelow
set splitright
set noshowmode
set noswapfile
set wrap linebreak nolist
set formatoptions+=r
set incsearch
set ignorecase
set smartcase
filetype plugin indent on
filetype plugin on

autocmd BufRead,BufNewFile *.html,*.css,*.js,*.json set tabstop=2
autocmd BufRead,BufNewFile *.html,*.css,*.js,*.json set softtabstop=2
autocmd BufRead,BufNewFile *.html,*.css,*.js,*.json set shiftwidth=2

" jump to last position when opening file
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

" Powerline setup
set laststatus=2
let g:airline_powerline_fonts=1
let g:airline_theme='luna'
"let g:airline_theme='dark'
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline_skip_empty_sections = 1
" set guifont=UbuntuMonoDerivativePowerline\ Nerd\ Font\ Regular\ 13

" NERDCommenter
let g:NERDCustomDelimiters = {
    \ 'haskell': { 'left': '-- ', 'nested': 1, 'leftAlt': '{- ', 'rightAlt': ' -}', 'nestedAlt': 1 },
    \ 'cabal': { 'left': '-- ' },
    \ 'c': { 'left': '// ', 'leftAlt': '/*', 'rightAlt': '*/' },
    \ 'cpp': { 'left': '// ', 'leftAlt': '/*', 'rightAlt': '*/' },
    \ 'javascript': { 'left': '// ', 'leftAlt': '/*', 'rightAlt': '*/' },
\ }

" fzf
let g:fzf_layout = { 'down': '~20%' }
nnoremap <C-p> :GFiles<CR>
nnoremap <leader>f :Files<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>s :Ag<CR>
nnoremap <leader>gc :Commits<CR>
nnoremap <leader>gd :BCommits<CR>

" GitGutter
nmap ]g <Plug>GitGutterNextHunk
nmap [g <Plug>GitGutterPrevHunk

if has("persistent_undo")
    set undodir=~/.vimundo/
    set undofile
endif

set background=dark
"colorscheme kalisi
colorscheme lucius
"highlight Normal ctermbg=234
highlight MatchParen cterm=reverse
highlight ExtraWhitespace ctermbg=88
match ExtraWhitespace /\s\+$/
highlight Comment cterm=italic gui=italic
highlight Normal ctermbg=NONE guibg=NONE
highlight haskellBottom ctermfg=Red
highlight haskellFail ctermfg=LightRed
highlight LineNr ctermfg=241 ctermbg=NONE

highlight GitGutterAdd ctermbg=none
highlight GitGutterChange ctermbg=none
highlight GitGutterChangeDelete ctermbg=none
highlight GitGutterDelete ctermbg=none
let g:gitgutter_map_keys = 0

" enable syntax for .ino files
au BufRead,BufNewFile *.pde,*.ino set filetype=cpp

" highlight if line exceeds specified amount
highlight ColorColumn ctermbg=236
au BufRead,BufNewFile *.py call matchadd('ColorColumn', '\%79v', 100) "set column nr

if exists("+mouse")
    set mouse=a
endif

"split navigations
nnoremap <C-Down> <C-W><C-J>
nnoremap <C-Up> <C-W><C-K>
nnoremap <C-Right> <C-W><C-L>
nnoremap <C-Left> <C-W><C-H>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

nnoremap Q <nop>
noremap <silent> <Up>   gk
noremap <silent> <Down> gj
noremap <silent> k gk
noremap <silent> j gj

"" various shortcuts
map <C-\> :TagbarToggle<CR>
nnoremap <C-w>' ciw''<Esc>P
nnoremap <C-w>" ciw""<Esc>P
noremap <leader>[ :nohlsearch<CR>
vnoremap > >gv
vnoremap < <gv

nnoremap <leader>q :bp<CR>
nnoremap <leader>w :bn<CR>

if !has('nvim')
    execute "set <M-o>=\eo"
endif
nnoremap <M-o> <C-w>w
vnoremap <M-o> <C-w>w

cnoreabbrev lo lopen
cnoreabbrev lc lclose

" lololol
nmap ; :
vmap ; :

" trying that
nmap , <leader>

