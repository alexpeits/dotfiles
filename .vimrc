set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'scrooloose/nerdtree.git'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'majutsushi/tagbar'
Plugin 'jmcantrell/vim-virtualenv'
"Plugin 'klen/python-mode'
Plugin 'Valloric/YouCompleteMe'
Plugin 'davidhalter/jedi-vim'
Plugin 'tpope/vim-fugitive'
Plugin 'Xuyuanp/nerdtree-git-plugin'
"Plugin 'Raimondi/delimitMate'
Plugin 'KabbAmine/zeavim.vim'
Plugin 'airblade/vim-gitgutter'
Plugin 'mattn/emmet-vim'
Plugin 'mbbill/undotree'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'mhartington/oceanic-next'
"Plugin 'nvie/vim-flake8'
Plugin 'tomasr/molokai'
Plugin 'chriskempson/base16-vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/syntastic'
Plugin 'Yggdroot/indentLine'
Plugin 'dhruvasagar/vim-table-mode'
call vundle#end()


"""""""""""""""""""""""""""""""""
" Powerline setup
set laststatus=2
let g:airline_powerline_fonts=1
"let g:airline_theme='hybridline'
let g:airline_theme='base16_eighties'
"let g:airline_theme='oceanicnext'
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#tabline#enabled = 1

" GitGutter setup
"let g:gitgutter_signs=0

" Virtualenv setup
let g:virtualenv_directory = $WORKON_HOME
let g:virtualenv_auto_activate = 1


"Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
let g:syntastic_loc_list_height=5
let g:syntastic_python_checkers = ["flake8"]

" YCM
nnoremap <leader>jd :YcmCompleter GoTo<CR>
nnoremap <leader>jd :YcmCompleter GoToReferences<CR>

" jedi-vim
let g:jedi#completions_enabled = 0
let g:jedi#popup_on_dot = 0
let g:jedi#smart_auto_mappings = 0

" IndentLine
let g:indentLine_color_term = 238


set shortmess+=I
set tabstop=4   
set softtabstop=4
set shiftwidth=4
au BufNewFile,BufRead *.js, *.html, *.css
    \ set tabstop=2
    \ set softtabstop=2
    \ set shiftwidth=2
set expandtab
set hidden
" set autoindent
filetype plugin indent on
filetype plugin on



if has("persistent_undo")
    set undodir=~/.vimundo/
    set undofile
endif
set foldmethod=indent
set foldlevel=99
set hlsearch
set autoread

" emacs-like parens jumping
set showmatch
set matchtime=4

"" remove delay from insert to normal
set timeoutlen=1000 ttimeoutlen=0

set updatetime=100

set nu
set background=dark
let python_highlight_all = 1
"let g:gruvbox_termtrans=1
"colorscheme sexy-railscasts-256
set t_Co=256
colorscheme OceanicNext2
"colorscheme gruvbox
"colorscheme onedark
"let g:rehash256 = 1
"colorscheme molokai
"colorscheme Tomorrow-Night-Eighties
highlight LineNr ctermbg=none ctermfg=241
highlight Normal ctermbg=none ctermfg=251
"highlight Function ctermfg=105
"highlight String ctermfg=78
highlight Search cterm=none ctermbg=222 ctermfg=234
highlight Error ctermbg=203
highlight VertSplit ctermbg=0
highlight MatchParen ctermbg=251 ctermfg=240
highlight Comment cterm=italic ctermfg=242

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
    set mouse=a     " Mice are wonderful.
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
"set splitbelow
"set splitright

"" run python script
nnoremap <F2> :w !python <CR>

"" tab navigation
nnoremap <M-PageUp>   <Esc>:tabprevious<CR>
nnoremap <M-PageDown> <Esc>:tabnext<CR>
nnoremap <C-M-PageUp> <Esc>:tabnew<CR>
nnoremap <C-M-PageDown> <Esc>:tabnew<CR>
nnoremap <leader>h :tabprevious<CR>
nnoremap <leader>l :tabnext<CR>

"" various shortcuts
map <F3> :NERDTreeToggle<CR>
map <F4> :TagbarToggle<CR>
"map <C-n> :set invnu <CR>
map <C-p> :PresentingStart<CR>
nnoremap <leader>0 :redraw!<CR>
nnoremap <F5> :GitGutterSignsToggle<CR>
nnoremap <F6> :UndotreeToggle<CR>
nnoremap <C-w>' ciw''<Esc>P
nnoremap <C-w>" ciw""<Esc>P
noremap <c-n> :nohlsearch<CR>
vnoremap > >gv
vnoremap < <gv

" lololol
nmap ; :

" flake8
autocmd FileType python map <buffer> <F8> :call Flake8()<CR>

" CtrlP
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

au BufRead,BufNewFile *.py vnoremap <silent> # :s#^#\##<cr>:noh<cr>
au BufRead,BufNewFile *.py vnoremap <silent> -# :s#^\###<cr>:noh<cr>

au BufRead,BufNewFile *.c,*.cpp,*.java,*.js vnoremap <silent> # :s#^#\//#<cr>:noh<cr>
au BufRead,BufNewFile *.c,*.cpp,*.java,*.js vnoremap <silent> -# :s#^\//##<cr>:noh<cr>

au BufRead,BufNewFile *.html vnoremap <silent> # :s/^\(.*\)$/<!-- \1 -->/<cr>:noh<cr>
au BufRead,BufNewFile *.html vnoremap <silent> -# :s/^<!--\s*\(.*\)\s*-->$/\1/<cr>:noh<cr>
" buffer navigation
nnoremap <leader>bq :bp <BAR> bd #<CR>
nnoremap <leader>bl :ls<CR>

" Various macros
let @d = "oimport ipdb; ipdb.set_trace()\e" " python debug trace

"""""""""""""""""""""""""""""""""

" python-mode
" Activate rope
" Keys:
" K             Show python docs
" <Ctrl-Space>  Rope autocomplete
" <Ctrl-c>g     Rope goto definition
" <Ctrl-c>d     Rope show documentation
" <Ctrl-c>f     Rope find occurrences
" <Leader>b     Set, unset breakpoint (g:pymode_breakpoint enabled)
" [[            Jump on previous class or function (normal, visual, operator modes)
" ]]            Jump on next class or function (normal, visual, operator modes)
" [M            Jump on previous class or method (normal, visual, operator modes)
" ]M            Jump on next class or method (normal, visual, operator modes)
let g:pymode_rope = 0
let g:pymode_rope_completion = 0
"let g:pymode_rope_rename_bind = '<C-c>rr'
"let g:pymode_rope_completion_bind = '<C-Space>'
let g:pymode_options_max_line_length = 0
if filereadable(".disable_rope")
    so .disable_rope
endif


" Documentation
let g:pymode_doc = 0
"let g:pymode_doc_key = 'K'
" If you prefer the Omni-Completion tip window to close when a selection is
" made, these lines close it on movement in insert mode or when leaving
" insert mode
"autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
"autocmd InsertLeave * if pumvisible() == 0|pclose|endif
set completeopt=menu
"Linting
let g:pymode_lint = 1
let g:pymode_lint_checker = "pyflakes,pep8,pylint"
"let g:pymode_options_max_line_length=79
" Auto check on save
let g:pymode_lint_on_write = 0
let g:pymode_lint_ignore = "E5"
"let g:pymode_lint_ignore = \"E5,W0401,E302,E225,E228,E265,E127,E128"

" Support virtualenv
let g:pymode_virtualenv = 1
let g:pymode_virtualenv_path = $VIRTUAL_ENV

" Enable breakpoints plugin
let g:pymode_breakpoint = 0
let g:pymode_breakpoint_bind = '<leader>b'

" syntax highlighting
let g:pymode_syntax = 0
let g:pymode_syntax_all = 1
let g:pymode_syntax_indent_errors = g:pymode_syntax_all
let g:pymode_syntax_space_errors = g:pymode_syntax_all

" Don't autofold code
let g:pymode_folding = 0

"let g:pymode_rope_show_doc_bind = 'K'


""""""""""""""""""""""""""""
