set nocompatible
filetype off
syntax enable

set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'scrooloose/nerdtree.git'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'majutsushi/tagbar'
Plugin 'davidhalter/jedi-vim'
"Plugin 'nvie/vim-flake8'
"Plugin 'Shougo/neocomplete'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'Raimondi/delimitMate'
Plugin 'airblade/vim-gitgutter'
Plugin 'mattn/emmet-vim'
Plugin 'mbbill/undotree'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'NLKNguyen/papercolor-theme'
Plugin 'freeo/vim-kalisi'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/syntastic'
Plugin 'Yggdroot/indentLine'
Plugin 'mileszs/ack.vim'
Plugin 'google/vim-searchindex'
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
let g:airline_theme='dark'
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#tabline#enabled = 1
" set guifont=UbuntuMonoDerivativePowerline\ Nerd\ Font\ Regular\ 13

"Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
let g:syntastic_loc_list_height = 5
let g:syntastic_enable_signs = 1
let g:syntastic_python_checkers = ["flake8"]

map <F8> :SyntasticCheck<CR>

" jedi-vim
"let g:jedi#completions_enabled = 0
"let g:jedi#popup_on_dot = 0
let g:jedi#smart_auto_mappings = 0
let g:jedi#use_splits_not_buffers = "winwidth"

" IndentLine
let g:indentLine_color_term = 238

" NERDCommenter
let g:NERDCustomDelimiters = {
    \ 'haskell': { 'left': '-- ', 'nested': 1, 'leftAlt': '{- ', 'rightAlt': ' -}', 'nestedAlt': 1 },
    \ 'cabal': { 'left': '-- ' },
    \ 'c': { 'left': '// ', 'leftAlt': '/*', 'rightAlt': '*/' },
    \ 'cpp': { 'left': '// ', 'leftAlt': '/*', 'rightAlt': '*/' },
    \ 'javascript': { 'left': '// ', 'leftAlt': '/*', 'rightAlt': '*/' },
\ }

" flake8
autocmd FileType python map <buffer> <F7> :call Flake8()<CR>

" tagbar
let g:tagbar_sort = 0

" CtrlP
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_user_command = 'ag %s -i --nogroup --hidden
\ --ignore .git
\ --ignore .svn
\ --ignore .hg
\ --ignore .DS_Store
\ --ignore node_modules
\ --ignore "**/*.pyc"
\ -g ""'
" let g:ctrlp_regexp = 1
let g:ctrlp_use_caching = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_switch_buffer = 0
" let g:ctrlp_match_func = {'match' : 'matcher#cmatch' }
let g:ackprg = 'ag --nogroup --column'
set grepprg=ag\ --nogroup\ --nocolor

" GitGutter
nmap ]g <Plug>GitGutterNextHunk
nmap [g <Plug>GitGutterPrevHunk


if has("persistent_undo")
    set undodir=~/.vimundo/
    set undofile
endif

set background=dark
"colorscheme PaperColor
colorscheme kalisi
highlight Normal ctermbg=235
highlight MatchParen cterm=reverse

highlight GitGutterAdd ctermbg=none
highlight GitGutterChange ctermbg=none
highlight GitGutterChangeDelete ctermbg=none
highlight GitGutterDelete ctermbg=none
let g:gitgutter_map_keys = 0

" enable syntax for .ino files
au BufRead,BufNewFile *.pde,*.ino set filetype=cpp

" highlight if line exceeds specified amount
highlight ColorColumn ctermbg=240
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
map <F3> :NERDTreeToggle<CR>
map <C-\> :TagbarToggle<CR>
nnoremap <leader>u :UndotreeToggle<CR>
nnoremap <C-w>' ciw''<Esc>P
nnoremap <C-w>" ciw""<Esc>P
noremap <leader>[ :nohlsearch<CR>
vnoremap > >gv
vnoremap < <gv

nnoremap <leader>q :bp<CR>
nnoremap <leader>w :bn<CR>

execute "set <M-o>=\eo"
nnoremap <M-o> <C-w>w

cnoreabbrev lo lopen
cnoreabbrev lc lclose

" lololol
nmap ; :
vmap ; :

au BufRead,BufNewFile *.py vnoremap <silent> # :s#^#\##<cr>:noh<cr>
au BufRead,BufNewFile *.py vnoremap <silent> -# :s#^\###<cr>:noh<cr>

au BufRead,BufNewFile *.html vnoremap <silent> # :s/^\(.*\)$/<!-- \1 -->/<cr>:noh<cr>
au BufRead,BufNewFile *.html vnoremap <silent> -# :s/^<!--\s*\(.*\)\s*-->$/\1/<cr>:noh<cr>

" Various macros
let @d = "oimport ipdb; ipdb.set_trace()\e" " python debug trace
