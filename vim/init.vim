"            (_)
"  _ ____   ___ _ __ ___  _ __ ___
" | '_ \ \ / / | '_ ` _ \| '__/ __|
" | | | \ V /| | | | | | | | | (__
" |_| |_|\_/ |_|_| |_| |_|_|  \___|
"
" Author: Mike Hartington
" repo  : https://github.com/mhartington/dotfiles/
"

" Setup NeoBundle  ----------------------------------------------------------{{{
" If vundle is not installed, do it first
if (!isdirectory(expand("$HOME/.config/nvim/bundle/neobundle.vim")))
 call system(expand("mkdir -p $HOME/.config/nvim/bundle"))
 call system(expand("git clone https://github.com/Shougo/neobundle.vim $HOME/.config/nvim/bundle/neobundle.vim"))
endif

set nocompatible

" Required:
set runtimepath+=~/.config/nvim/bundle/neobundle.vim/
" set runtimepath+=~/Github/deoplete-angular/

" Required:
call neobundle#begin(expand('~/.config/nvim/bundle/'))
let pluginsExist = 0
" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" syntax
NeoBundleLazy 'elzr/vim-json', {'autoload':{'filetypes':['json']}}
NeoBundle 'tpope/vim-markdown'
" colorscheme & syntax highlighting
NeoBundle 'tomasr/molokai'
NeoBundle 'NLKNguyen/papercolor-theme'
NeoBundle 'rakr/vim-one'
NeoBundle 'Yggdroot/indentLine'
NeoBundle 'Raimondi/delimitMate'
NeoBundle 'mxw/vim-jsx'
NeoBundle 'othree/yajs.vim'
" Git helpers
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'Xuyuanp/nerdtree-git-plugin'
NeoBundle 'google/vim-searchindex'
" utils
NeoBundle 'benekastah/neomake'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'jistr/vim-nerdtree-tabs'
NeoBundle 'christoomey/vim-tmux-navigator'
NeoBundle 'tmux-plugins/vim-tmux'
NeoBundle 'tmux-plugins/vim-tmux-focus-events'
NeoBundle 'vim-airline/vim-airline'
NeoBundle 'vim-airline/vim-airline-themes'
NeoBundle 'tpope/vim-surround'
NeoBundle 'mattn/emmet-vim'
NeoBundle 'ap/vim-css-color'
NeoBundle 'ctrlpvim/ctrlp.vim'
NeoBundle 'mbbill/undotree'
NeoBundle 'scrooloose/nerdcommenter'
NeoBundle 'davidhalter/jedi-vim'
NeoBundle 'majutsushi/tagbar'
NeoBundle 'mileszs/ack.vim'
NeoBundle 'vim-expand-region'
NeoBundle 'michaeljsmith/vim-indent-object'
NeoBundle 'neovimhaskell/haskell-vim'
NeoBundle 'dag/vim2hs'
NeoBundle 'ntpeters/vim-better-whitespace'

" Shougo
NeoBundle 'Shougo/deoplete.nvim'
NeoBundle 'zchee/deoplete-jedi'
NeoBundle 'zchee/deoplete-clang'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/neoinclude.vim'

NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'Shougo/neosnippet-snippets'

call neobundle#end()

" Required:
filetype plugin indent on
filetype plugin on

let pluginsExist=1
NeoBundleCheck
" }}}

if pluginsExist
" System Settings  ----------------------------------------------------------{{{


let g:python_host_prog = '/usr/bin/python'

" Neovim Settings
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
" Let airline tell me my status
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
let g:gitgutter_max_signs = 1000  " default value

" }}}

" System mappings  ----------------------------------------------------------{{{

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

" terminal
autocmd BufWinEnter,WinEnter term://* startinsert
autocmd BufLeave term://* stopinsert
"tnoremap <leader><ESC> <C-\><C-n><C-w><C-p>
tnoremap <C-g> <C-\><C-n><C-w><C-p>

let g:terminal_color_0= "#181818"
let g:terminal_color_1= "#e33b3b"
let g:terminal_color_2= "#6fb929"
let g:terminal_color_3= "#c0a21d"
let g:terminal_color_4= "#729fcf"
let g:terminal_color_5= "#ac63b8"
let g:terminal_color_6= "#27c5c5"
let g:terminal_color_7= "#d8d8d8"
let g:terminal_color_8= "#585858"
let g:terminal_color_9= "#e33b3b"
let g:terminal_color_10="#8ae234"
let g:terminal_color_11="#fce94f"
let g:terminal_color_12="#729fcf"
let g:terminal_color_13="#ac63b8"
let g:terminal_color_14="#27c5c5"
let g:terminal_color_15="#f8f8f8"
let g:terminal_color_background="#1b2b34"
let g:terminal_color_foreground="#c1c6cf"
"}}}"

" Themes, Commands, etc  ----------------------------------------------------{{{
" Theme
syntax enable
set termguicolors
" no need to fold things in markdown all the time
let g:vim_markdown_folding_disabled = 1
" turn on spelling for markdown files
"autocmd BufRead,BufNewFile *.md,*.rst setlocal spell complete+=kspell
" highlight bad words in red
"autocmd BufRead,BufNewFile *.md,*.rst hi SpellBad guibg=#3A3A3A" ctermbg=224
" disable markdown auto-preview. Gets annoying
let g:instant_markdown_autostart = 0
" Keep my termo window open when I navigate away
autocmd TermOpen * set bufhidden=hide

autocmd BufRead,BufNewFile rfc*.txt set ft=rfc

set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set hidden
set lazyredraw
autocmd BufRead,BufNewFile *.js,*.html,*.css set tabstop=2
autocmd BufRead,BufNewFile *.js,*.html,*.css set softtabstop=2
autocmd BufRead,BufNewFile *.js,*.html,*.css set shiftwidth=2

au BufRead,BufNewFile */nginx/*.conf set ft=nginx
map <F10> :set invlazyredraw<CR>

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

" auto comment lines after comments
set formatoptions+=r

set updatetime=100

" syntax for .todo files
au BufRead,BufNewFile *.todo set filetype=rst

set nu


if $VIMCOLOR == 1
    "colorscheme flattened_light
    so $HOME/.config/nvim/colorschemes/kalisi_light.vim
    highlight VertSplit ctermbg=239 ctermfg=246 guibg=none guifg=#777777
else
    "colorscheme flattened_dark
    set background=dark
    colorscheme PaperColor
    let g:airline_theme='molokai'
    "so $HOME/.config/nvim/colorschemes/kalisi.vim
    "so $HOME/.config/nvim/colorschemes/molokai.vim
endif

highlight Error gui=bold,underline guifg=#e5a5a5 guibg=#602020
hi! link ExtraWhitespace Error

highlight GitGutterAdd ctermbg=none guibg=#272727
highlight GitGutterChange ctermbg=none guibg=#272727
highlight GitGutterChangeDelete ctermbg=none guibg=#272727
highlight GitGutterDelete ctermbg=none guibg=#272727
let g:gitgutter_map_keys = 0

" vimdiff
highlight DiffAdd ctermbg=none guibg=none
highlight DiffAdded ctermbg=none guibg=none
highlight DiffChange ctermbg=none guibg=none
highlight DiffDelete ctermbg=none guibg=none
highlight DiffText ctermbg=none guibg=none
highlight DiffFile ctermbg=none guibg=none
highlight DiffLine ctermbg=none guibg=none
highlight DiffNewFile ctermbg=none guibg=none
highlight DiffRemoved ctermbg=none guibg=none


" highlight if line exceeds specified amount
if $MAXLEN
    au BufRead,BufNewFile *.py call matchadd('ColorColumn', '\%' . $MAXLEN . 'v', 100) "set column nr
else
    au BufRead,BufNewFile *.py call matchadd('ColorColumn', '\%80v', 100) "set column nr
endif

"}}}

" Fold, gets it's own section  ----------------------------------------------{{{

function! MyFoldText() " {{{
    let line = getline(v:foldstart)

    let nucolwidth = &fdc + &number * &numberwidth
    let windowwidth = winwidth(0) - nucolwidth - 3
    let foldedlinecount = v:foldend - v:foldstart

    " expand tabs into spaces
    let onetab = strpart('          ', 0, &tabstop)
    let line = substitute(line, '\t', onetab, 'g')

    let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
    let fillcharcount = windowwidth - len(line) - len(foldedlinecount)
    return line . '…' . repeat(" ",fillcharcount) . foldedlinecount . '…' . ' '
endfunction " }}}

set foldtext=MyFoldText()

autocmd InsertEnter * if !exists('w:last_fdm') | let w:last_fdm=&foldmethod | setlocal foldmethod=manual | endif
autocmd InsertLeave,WinLeave * if exists('w:last_fdm') | let &l:foldmethod=w:last_fdm | unlet w:last_fdm | endif

autocmd FileType vim setlocal fdc=1
set foldlevel=99
" Space to toggle folds.
nnoremap <Space> za
vnoremap <Space> za
autocmd FileType vim setlocal foldmethod=marker
autocmd FileType vim setlocal foldlevel=0

autocmd FileType html setlocal fdl=99

" }}}

" Tab for completion -----------------------------------------------------------------{{{

" SuperTab like snippets behavior.
imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: pumvisible() ? "\<C-n>" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: "\<TAB>"

"}}}

" Packages -----------------------------------------------------------------{{{

let g:jsx_ext_required = 0 " Allow JSX in normal JS files

" NERDTree
autocmd StdinReadPre * let s:std_in=1
let NERDTreeShowHidden=1
let g:NERDTreeWinSize=30
let g:NERDTreeAutoDeleteBuffer=1

" jedi-vim
let g:jedi#completions_enabled = 0
let g:jedi#popup_on_dot = 0
let g:jedi#smart_auto_mappings = 0
if $PYTHONCURRENT == '3'
    let g:jedi#force_py_version = 3
endif
"
" `goto` splits the window vertically if there is enough space
" (from `textwidth`), else horizontally
"
let g:jedi#use_splits_not_buffers = "winwidth"

" deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#clang#libclang_path = '/usr/lib/llvm-3.8/lib/libclang.so'
let g:deoplete#sources#clang#clang_header = '/usr/lib/clang'
autocmd CompleteDone * pclose!
nnoremap <F7> :call deoplete#refresh()<CR>
inoremap <F7> <C-o>:call deoplete#refresh()<CR>
inoremap <silent><expr> <F9>
        \ pumvisible() ? "\<C-n>" :
        \ <SID>check_back_space() ? "\<TAB>" :
        \ deoplete#mappings#manual_complete()
        function! s:check_back_space() abort "{{{
        let col = col('.') - 1
        return !col || getline('.')[col - 1]  =~ '\s'
        endfunction"}}}

" deoplete-jedi
let g:python_host_prog = '/home/alex/.virtualenvs/_nv_py2/bin/python'
let g:python3_host_prog = '/home/alex/.virtualenvs/_nv_py3/bin/python'
"let deoplete#sources#jedi#enable_cache=0

" IndentLine
let g:indentLine_color_term = 236
let g:indentLine_color_gui = '#373737'
if $VIMCOLOR == 1
    let g:indentLine_color_term = 250
    let g:indentLine_color_gui = '#c1c1c1'
endif
"let g:indentLine_char = │

" CtrlP
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP .'
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CTRLP & GREP
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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

" Tagbar
let g:tagbar_sort = 0
let g:tagbar_type_rst = {
    \ 'ctagstype': 'rst',
    \ 'ctagsbin' : '~/.config/nvim/custom/rst2ctags.py',
    \ 'ctagsargs' : '-f - --sort=yes',
    \ 'kinds' : [
        \ 's:sections',
        \ 'i:images'
    \ ],
    \ 'sro' : '|',
    \ 'kind2scope' : {
        \ 's' : 'section',
    \ },
    \ 'sort': 0,
\ }
let g:tagbar_type_haskell = {
    \ 'ctagsbin'  : 'hasktags',
    \ 'ctagsargs' : '-x -c -o-',
    \ 'kinds'     : [
        \  'm:modules:0:1',
        \  'd:data: 0:1',
        \  'd_gadt: data gadt:0:1',
        \  't:type names:0:1',
        \  'nt:new types:0:1',
        \  'c:classes:0:1',
        \  'cons:constructors:1:1',
        \  'c_gadt:constructor gadt:1:1',
        \  'c_a:constructor accessors:1:1',
        \  'ft:function types:1:1',
        \  'fi:function implementations:0:1',
        \  'o:others:0:1'
    \ ],
    \ 'sro'        : '.',
    \ 'kind2scope' : {
        \ 'm' : 'module',
        \ 'c' : 'class',
        \ 'd' : 'data',
        \ 't' : 'type'
    \ },
    \ 'scope2kind' : {
        \ 'module' : 'm',
        \ 'class'  : 'c',
        \ 'data'   : 'd',
        \ 'type'   : 't'
    \ }
\ }

" nerd commenter
let g:NERDCustomDelimiters = {
    \ 'haskell': { 'left': '{- ','right': ' -}', 'nested': 1, 'leftAlt': '-- ', 'nestedAlt': 1 },
    \ 'cabal': { 'left': '-- ' },
    \ 'c': { 'left': '// ', 'leftAlt': '/*', 'rightAlt': '*/' },
    \ 'cpp': { 'left': '// ', 'leftAlt': '/*', 'rightAlt': '*/' },
    \ 'javascript': { 'left': '// ', 'leftAlt': '/*', 'rightAlt': '*/' },
\ }

" GitGutter
nmap ]g <Plug>GitGutterNextHunk
nmap [g <Plug>GitGutterPrevHunk

" haskell-vim
let g:haskell_indent_if = 0
let g:haskell_indent_case = 4
let g:haskell_indent_case_alternative = 4
let g:haskell_indent_where = 2
let g:haskell_indent_in = 4
let g:haskell_indent_do = 4

" delimitMate
let g:delimitMate_expand_cr = 1

"}}}

" Emmet customization -------------------------------------------------------{{{
" Enable Emmet in all modes
  function! s:expand_html_tab()
" try to determine if we're within quotes or tags.
" if so, assume we're in an emmet fill area.
   let line = getline('.')
   if col('.') < len(line)
     let line = matchstr(line, '[">][^<"]*\%'.col('.').'c[^>"]*[<"]')
     if len(line) >= 2
        return "\<C-n>"
     endif
   endif
" expand anything emmet thinks is expandable.
  if emmet#isExpandable()
    return "\<C-y>,"
  endif
" return a regular tab character
  return "\<tab>"
  endfunction
  autocmd FileType html,markdown imap <buffer><expr><tab> <sid>expand_html_tab()
  let g:user_emmet_mode='a'
  let g:user_emmet_complete_tag = 1
  let g:user_emmet_install_global = 0
  autocmd FileType html,css,js,javascript.jsx EmmetInstall
"}}}

" Navigate between vim buffers and tmux panels ------------------------------{{{
let g:tmux_navigator_no_mappings = 1
nnoremap <silent> <C-j> :TmuxNavigateDown<cr>
nnoremap <silent> <C-k> :TmuxNavigateUp<cr>
nnoremap <silent> <C-l> :TmuxNavigateRight<cr>
nnoremap <silent> <C-h> :TmuxNavigateLeft<CR>
nnoremap <silent> <C-;> :TmuxNavigatePrevious<cr>
"}}}

" vim-airline ---------------------------------------------------------------{{{
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#branch#enabled = 1
"set hidden
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tabline#show_tab_nr = 1
let g:airline_powerline_fonts = 1
"let g:airline_theme='hybridline'
"let g:airline_theme='spacemacs'
"let g:airline_theme='oceanicnext'
"let g:airline_theme='base16_eighties'
"let g:airline_theme='alexline'
let g:airline_symbols = {}
let g:airline_symbols.branch = '⎇ '
"tmap <leader>x <c-\><c-n>:bp! <BAR> bd! #<CR>
nmap <leader>t :term<cr>
let g:airline#extensions#tabline#buffer_idx_mode = 1
tmap <leader>1  <C-\><C-n><Plug>AirlineSelectTab1
tmap <leader>2  <C-\><C-n><Plug>AirlineSelectTab2
tmap <leader>3  <C-\><C-n><Plug>AirlineSelectTab3
tmap <leader>4  <C-\><C-n><Plug>AirlineSelectTab4
tmap <leader>5  <C-\><C-n><Plug>AirlineSelectTab5
tmap <leader>6  <C-\><C-n><Plug>AirlineSelectTab6
tmap <leader>7  <C-\><C-n><Plug>AirlineSelectTab7
tmap <leader>8  <C-\><C-n><Plug>AirlineSelectTab8
tmap <leader>9  <C-\><C-n><Plug>AirlineSelectTab9
nmap <leader>1 <Plug>AirlineSelectTab1
nmap <leader>2 <Plug>AirlineSelectTab2
nmap <leader>3 <Plug>AirlineSelectTab3
nmap <leader>4 <Plug>AirlineSelectTab4
nmap <leader>5 <Plug>AirlineSelectTab5
nmap <leader>6 <Plug>AirlineSelectTab6
nmap <leader>7 <Plug>AirlineSelectTab7
nmap <leader>8 <Plug>AirlineSelectTab8
nmap <leader>9 <Plug>AirlineSelectTab9
set encoding=utf8
set guifont=UbuntuMonoDerivativePowerline\ Nerd\ Font\ Regular\ 11
"}}}

" Linting -------------------------------------------------------------------{{{

let g:neomake_python_venvpylint_maker = {
  \ 'exe': 'python',
  \ 'args': [
      \ '`which pylint`',
      \ '-f', 'text',
      \ '--msg-template="{path}:{line}:{column}:{C}: [{symbol}] {msg}"',
      \ '-r', 'n'
  \ ],
  \ 'errorformat':
      \ '%A%f:%l:%c:%t: %m,' .
      \ '%A%f:%l: %m,' .
      \ '%A%f:(%l): %m,' .
      \ '%-Z%p^%.%#,' .
      \ '%-G%.%#',
  \ }

let g:neomake_python_enabled_makers = ['venvpylint', 'flake8']
let g:neomake_java_enabled_makers = []
autocmd! BufWritePost * Neomake
let g:neomake_open_list = 0
let g:neomake_verbose = 0

highlight NeomakeWarning ctermfg=223 ctermbg=none guifg=#ffd7af guibg=#303030
highlight NeomakeError ctermfg=203 ctermbg=none guifg=#ff5f5f guibg=#303030
let g:neomake_warning_sign={'text': '⚠', 'texthl': 'NeomakeWarning'}
let g:neomake_error_sign={'text': '✖', 'texthl': 'NeomakeError'}

"}}}
endif

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
