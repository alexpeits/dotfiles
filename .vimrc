set nocompatible
filetype off
syntax on

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'scrooloose/nerdtree.git'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'majutsushi/tagbar'
"Plugin 'jmcantrell/vim-virtualenv'
"Plugin 'klen/python-mode'
Plugin 'Valloric/YouCompleteMe'
Plugin 'davidhalter/jedi-vim'
Plugin 'tpope/vim-fugitive'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'Raimondi/delimitMate'
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
"Plugin 'benekastah/neomake'
Plugin 'scrooloose/syntastic'
Plugin 'Yggdroot/indentLine'
Plugin 'dhruvasagar/vim-table-mode'
"Plugin 'ervandew/supertab'
"Plugin 'Rykka/InstantRst'
"Plugin 'Shougo/neocomplete.vim'
Plugin 'rust-lang/rust.vim', {'for': 'rust'}
Plugin 'cespare/vim-toml'
Plugin 'fatih/vim-go'
call vundle#end()


"""""""""""""""""""""""""""""""""
"let g:neocomplete#enable_at_startup = 1

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
"let g:virtualenv_directory = $WORKON_HOME
"let g:virtualenv_auto_activate = 1


"Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
let g:syntastic_loc_list_height=5
let g:syntastic_python_checkers = ["pylint"]
let g:syntastic_enable_signs=1
map <F8> :SyntasticCheck<CR>

" YCM
let g:ycm_collect_identifiers_from_tags_files = 1 " Let YCM read tags from Ctags file
let g:ycm_use_ultisnips_completer = 1 " Default 1, just ensure
let g:ycm_seed_identifiers_with_syntax = 1 " Completion for programming language's keyword
let g:ycm_complete_in_comments = 1 " Completion in comments
let g:ycm_complete_in_strings = 1 " Completion in string
let g:ycm_min_num_of_chars_for_completion = 1
let g:ycm_auto_trigger = 1
nnoremap <leader>jd :YcmCompleter GoTo<CR>
nnoremap <leader>jr :YcmCompleter GoToReferences<CR>
if $PYTHONCURRENT == '3'
    let g:ycm_python_binary_path = '/usr/bin/python3'
endif
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_global_ycm_extra_conf = '~/.vim/confs/.ycm_extra_conf.py'

" jedi-vim
let g:jedi#completions_enabled = 0
let g:jedi#popup_on_dot = 0
let g:jedi#smart_auto_mappings = 0
if $PYTHONCURRENT == '3'
    let g:jedi#force_py_version = 3
endif

" IndentLine
let g:indentLine_color_term = 240

" Delimitmate
let g:loaded_delimitMate = 0
au FileType html,css,c,js let b:loaded_delimitMate = 1

" neomake
"let g:neomake_python_enabled_makers = ['flake8']
"map <F8> :Neomake<CR>

set shortmess+=I
set tabstop=4   
set softtabstop=4
set shiftwidth=4
"au BufNewFile,BufRead *.js, *.html, *.css
    "\ set tabstop=2
    "\ set softtabstop=2
    "\ set shiftwidth=2
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

nnoremap <leader>] :set invrelativenumber<cr>
autocmd BufLeave,WinLeave,FocusLost * :set norelativenumber 
autocmd BufEnter,WinEnter,FocusGained * :set relativenumber
autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber

set number
set relativenumber
set background=dark
let python_highlight_all = 1
"let g:gruvbox_termtrans=1
"colorscheme sexy-railscasts-256
set t_Co=256
"colorscheme OceanicNext2
"colorscheme gruvbox
"colorscheme onedark
"let g:rehash256 = 1
"colorscheme molokai
"colorscheme Tomorrow-Night
"colorscheme elflord
"colorscheme wombat256
let base16colorspace=256
colorscheme base16-default-dark
so $HOME/.config/nvim/custom/base16colors.vim
"colorscheme codeschool
highlight LineNr ctermbg=none ctermfg=241
highlight CursorLineNr ctermbg=239 ctermfg=245
highlight Normal ctermbg=none ctermfg=251
"highlight Function ctermfg=105
"highlight String ctermfg=78
highlight Search cterm=none ctermbg=222 ctermfg=234
highlight Error ctermbg=203 cterm=none
highlight VertSplit ctermbg=239 ctermfg=246
"highlight MatchParen ctermbg=251 ctermfg=240 cterm=none
highlight Comment cterm=italic ctermfg=244
highlight Todo cterm=italic ctermbg=114 ctermfg=234
"highlight pythonSelf ctermfg=223
"highlight pythonClass ctermfg=147
if exists('$TMUX')
    highlight Comment cterm=none ctermfg=242
else
    highlight Comment cterm=italic ctermfg=243
endif

" elflord
"highlight String ctermfg=5
"highlight Structure ctermfg=2
"highlight Function ctermfg=4
"highlight pythonClass ctermfg=6
"highlight pythonSelf ctermfg=217
"highlight pythonDoctest ctermfg=2
"highlight MatchParen ctermbg=237 ctermfg=180 cterm=underline
"highlight SpellBad cterm=underline ctermbg=237
"highlight SpellCap cterm=none ctermbg=none
"highlight SpellLocal cterm=none ctermbg=none
"highlight SpellRare cterm=none ctermbg=none
"highlight rstSections ctermfg=210 cterm=bold
"highlight rstStrongEmphasis cterm=bold
"highlight Pmenu ctermbg=238 ctermfg=248
"highlight PmenuSel ctermbg=248 ctermfg=236

" Codeschool
"highlight pythonSelf ctermfg=174
"highlight pythonClass ctermfg=174
"highlight pythonDoctest ctermfg=209
"highlight NonText ctermbg=none
"highlight Folded ctermbg=238 ctermfg=246
"highlight Pmenu ctermbg=238
"highlight SpellBad cterm=underline ctermbg=237
"highlight SpellCap cterm=none ctermbg=none
"highlight SpellLocal cterm=none ctermbg=none
"highlight SpellRare cterm=none ctermbg=none
"highlight rstSections ctermfg=210 cterm=bold

highlight GitGutterAdd ctermbg=none
highlight GitGutterChange ctermbg=none
highlight GitGutterChangeDelete ctermbg=none
highlight GitGutterDelete ctermbg=none
let g:gitgutter_map_keys = 0

" vimdiff
highlight DiffAdd ctermbg=none
highlight DiffAdded ctermbg=none
highlight DiffChange ctermbg=none
highlight DiffDelete ctermbg=none
highlight DiffText ctermbg=none
highlight DiffFile ctermbg=none
highlight DiffLine ctermbg=none
highlight DiffNewFile ctermbg=none
highlight DiffRemoved ctermbg=none

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
map <C-\> :TagbarToggle<CR>
"map <C-n> :set invnu <CR>
map <C-p> :PresentingStart<CR>
nnoremap <leader>0 :redraw!<CR>
nnoremap <F5> :GitGutterSignsToggle<CR>
nnoremap <leader>u :UndotreeToggle<CR>
nnoremap <C-w>' ciw''<Esc>P
nnoremap <C-w>" ciw""<Esc>P
noremap <leader>[ :nohlsearch<CR>
vnoremap > >gv
vnoremap < <gv
nnoremap <F9> :SyntasticToggleMode<CR>

" lololol
nmap ; :

" flake8
"autocmd FileType python map <buffer> <F8> :call Flake8()<CR>

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
