set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'scrooloose/nerdtree.git'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'majutsushi/tagbar'
"Plugin 'davidhalter/jedi-vim'
Plugin 'NLKNguyen/papercolor-theme'
Plugin 'tpope/vim-fugitive'
Plugin 'Raimondi/delimitMate'
Plugin 'scrooloose/nerdcommenter'
Plugin 'airblade/vim-gitgutter'
Plugin 'mattn/emmet-vim'
Plugin 'ctrlpvim/ctrlp.vim'
call vundle#end()

set noshowmode
set noswapfile
filetype on
set splitright
set splitbelow
set backspace=2
set number

" jump to last position when opening file
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

set conceallevel=0
let g:tex_conceal = ""
" block select not limited by shortest line
set virtualedit=block
set wildmenu
set wildmode=full
set laststatus=2
"set colorcolumn=100
set wrap linebreak nolist

" No need for ex mode
nnoremap Q <nop>
" Navigate between display lines
noremap  <silent> <Up>   gk
noremap  <silent> <Down> gj
noremap  <silent> k gk
noremap  <silent> j gj
noremap  <silent> <Home> g<Home>
noremap  <silent> <End>  g<End>
inoremap <silent> <Home> <C-o>g<Home>
inoremap <silent> <End>  <C-o>g<End>


syntax enable
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set hidden
set lazyredraw
autocmd BufRead,BufNewFile *.js,*.html,*.css set tabstop=2
autocmd BufRead,BufNewFile *.js,*.html,*.css set softtabstop=2
autocmd BufRead,BufNewFile *.js,*.html,*.css set shiftwidth=2

if has("persistent_undo")
    set undodir=~/.vimundo/
    set undofile
endif
set foldmethod=indent
set foldlevel=99
set hlsearch
set autoread
set autoindent
filetype plugin indent on

" emacs-like parens jumping
set showmatch
set matchtime=4

"" remove delay from insert to normal
set timeoutlen=1000 ttimeoutlen=0

" auto comment lines after comments
set formatoptions+=r

set updatetime=100

set nu

colorscheme PaperColor

highlight Error gui=bold,underline guifg=#e5a5a5 guibg=#602020
hi! link ExtraWhitespace Error

highlight GitGutterAdd ctermbg=none guibg=#272727
highlight GitGutterChange ctermbg=none guibg=#272727
highlight GitGutterChangeDelete ctermbg=none guibg=#272727
highlight GitGutterDelete ctermbg=none guibg=#272727
let g:gitgutter_map_keys = 0



" CtrlP
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP .'
if executable('ag')
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
endif

" nerd commenter
let g:NERDCustomDelimiters = {
    \ 'haskell': { 'left': '{- ','right': ' -}', 'nested': 1, 'leftAlt': '-- ', 'nestedAlt': 1 },
    \ 'cabal': { 'left': '-- ' },
    \ 'c': { 'left': '// ', 'leftAlt': '/*', 'rightAlt': '*/' },
    \ 'cpp': { 'left': '// ', 'leftAlt': '/*', 'rightAlt': '*/' },
    \ 'javascript': { 'left': '// ', 'leftAlt': '/*', 'rightAlt': '*/' },
\ }

" haskell-vim
let g:haskell_indent_if = 0
let g:haskell_indent_case = 4
let g:haskell_indent_case_alternative = 4
let g:haskell_indent_where = 2
let g:haskell_indent_in = 4
let g:haskell_indent_do = 4


" vim-airline
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#branch#enabled = 1
"set hidden
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tabline#show_tab_nr = 1
let g:airline_powerline_fonts = 1
let g:airline_theme='dark'
let g:airline_symbols = {}
let g:airline_symbols.branch = '⎇ '
"tmap <leader>x <c-\><c-n>:bp! <BAR> bd! #<CR>
nmap <leader>t :term<cr>
let g:airline#extensions#tabline#buffer_idx_mode = 1
set encoding=utf8
"set guifont=UbuntuMonoDerivativePowerline\ Nerd\ Font\ Regular\ 11

"split navigations
nnoremap <C-Down> <C-W><C-J>
nnoremap <C-Up> <C-W><C-K>
nnoremap <C-Right> <C-W><C-L>
nnoremap <C-Left> <C-W><C-H>

"" various shortcuts
nnoremap <leader>f :NERDTreeTabsToggle<CR>
map <C-\> :TagbarToggle<CR>
nnoremap <F5> :GitGutterSignsToggle<CR>
nnoremap <leader>u :UndotreeToggle<CR>
nnoremap <C-w>' ciw''<Esc>P
nnoremap <C-w>" ciw""<Esc>P
nnoremap <C-w>( ciw()<Esc>P
nnoremap <leader>[ :noh<CR>
nnoremap <leader>= :cn<CR>zz
nnoremap <leader>- :cp<CR>zz
nnoremap <leader>+ :copen<CR>
nnoremap <leader>_ :cclose<CR>
nnoremap <leader>el :lopen<CR>
"nnoremap <leader>a <Esc>:Ack!<CR>
nnoremap <F4> :set invwrap<CR>
vnoremap > >gv
vnoremap < <gv

" lololol
nmap ; :
vmap ; :

" buffer navigation
nnoremap <leader>h :bp<CR>
nnoremap <leader>l :bn<CR>
nnoremap <leader>bq :bp <BAR> bd #<CR>
nnoremap <leader>bb :ls<CR>
nnoremap <leader>q :bp<CR>
nnoremap <leader>w :bn<CR>

" tab navigation
nnoremap <leader>j :tabp<CR>
nnoremap <leader>k :tabn<CR>

" Various macros
let @d = "oimport ipdb; ipdb.set_trace()\e"  " python debug trace
