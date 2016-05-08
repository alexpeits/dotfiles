set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'scrooloose/nerdtree.git'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'majutsushi/tagbar'
"Plugin 'klen/python-mode'
"Plugin 'Valloric/YouCompleteMe'
Plugin 'davidhalter/jedi-vim'
Plugin 'tpope/vim-fugitive'
"Plugin 'Raimondi/delimitMate'
Plugin 'KabbAmine/zeavim.vim'
Plugin 'airblade/vim-gitgutter'
Plugin 'mattn/emmet-vim'
Plugin 'mbbill/undotree'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'jmcantrell/vim-virtualenv'
call vundle#end()


"""""""""""""""""""""""""""""""""

" Powerline setup
set laststatus=2
"set ttimeoutlen=50
let g:airline_powerline_fonts=1
"let g:airline_theme='hybridline'
let g:airline_theme='base16_eighties'
let g:airline#extensions#branch#enabled = 1

" GitGutter setup
"let g:gitgutter_signs=0

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
let g:pymode_doc = 1
let g:pymode_doc_key = 'K'
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
let g:virtualenv_directory = $WORKON_HOME
let g:pymode_virtualenv_path = $VIRTUAL_ENV

" Enable breakpoints plugin
let g:pymode_breakpoint = 1
let g:pymode_breakpoint_bind = '<leader>b'

" syntax highlighting
let g:pymode_syntax = 1
let g:pymode_syntax_all = 1
let g:pymode_syntax_indent_errors = g:pymode_syntax_all
let g:pymode_syntax_space_errors = g:pymode_syntax_all

" Don't autofold code
let g:pymode_folding = 0

let g:pymode_rope_show_doc_bind = 'K'


""""""""""""""""""""""""""""


set shortmess+=I
set tabstop=8
set expandtab
set softtabstop=4
set shiftwidth=4
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
"colorscheme gruvbox
"colorscheme sexy-railscasts-256
set t_Co=256
colorscheme OceanicNext
highlight LineNr ctermbg=none ctermfg=241
highlight Function ctermfg=105
highlight Normal ctermbg=none ctermfg=251
highlight Search cterm=none ctermbg=222 ctermfg=234
highlight Error ctermbg=160
highlight MatchParen ctermbg=251 ctermfg=240
highlight Comment cterm=italic ctermfg=242

highlight GitGutterAdd ctermbg=none
highlight GitGutterChange ctermbg=none ctermfg=221
highlight GitGutterChangeDelete ctermbg=none
highlight GitGutterDelete ctermbg=none


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

"" run python script
nnoremap <F2> :w !python <CR>

"" tab navigation
nnoremap <M-PageUp>   <Esc>:tabprevious<CR>
nnoremap <M-PageDown> <Esc>:tabnext<CR>
nnoremap <C-M-PageUp> <Esc>:tabnew<CR>
nnoremap <C-M-PageDown> <Esc>:tabnew<CR>

"" various shortcuts
map <F3> :NERDTreeToggle<CR>
map <F4> :TagbarToggle<CR>
map <C-n> :set invnu <CR>
map <C-p> :PresentingStart<CR>
nnoremap <C-L> :redraw!<CR>
nnoremap <F5> :GitGutterSignsToggle<CR>
nnoremap <F6> :UndotreeToggle<CR>
nnoremap <F8> :PymodeLint<CR>
" CtrlP
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

au BufRead,BufNewFile *.py vnoremap <silent> # :s#^#\##<cr>:noh<cr>
au BufRead,BufNewFile *.py vnoremap <silent> -# :s#^\###<cr>:noh<cr>

au BufRead,BufNewFile *.c,*.cpp,*.java,*.js vnoremap <silent> # :s#^#\//#<cr>:noh<cr>
au BufRead,BufNewFile *.c,*.cpp,*.java,*.js vnoremap <silent> -# :s#^\//##<cr>:noh<cr>

au BufRead,BufNewFile *.html vnoremap <silent> # :s/^\(.*\)$/<!-- \1 -->/<cr>:noh<cr>
au BufRead,BufNewFile *.html vnoremap <silent> -# :s/^<!--\s*\(.*\)\s*-->$/\1/<cr>:noh<cr>
