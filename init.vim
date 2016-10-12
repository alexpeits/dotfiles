"|  \/  (_) |                        (_)
"| .  . |_| | _____  ___   _ ____   ___ _ __ ___  _ __ ___
"| |\/| | | |/ / _ \/ __| | '_ \ \ / / | '_ ` _ \| '__/ __|
"| |  | | |   <  __/\__ \ | | | \ V /| | | | | | | | | (__
"\_|  |_/_|_|\_\___||___/ |_| |_|\_/ |_|_| |_| |_|_|  \___|
"
" Author: Mike Hartington
" repo  : https://github.com/mhartington/dotfiles/
"

" Setup NeoBundle  ----------------------------------------------------------{{{
" If vundle is not installed, do it first
if (!isdirectory(expand("$HOME/.config/nvim/bundle/neobundle.vim")))
 call system(expand("mkdir -p $HOME/.confg/nvim/bundle"))
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
"NeoBundle 'junegunn/limelight.vim'
"NeoBundle 'dhruvasagar/vim-table-mode'
NeoBundle 'suan/vim-instant-markdown'
" colorscheme & syntax highlighting
NeoBundle 'mhartington/oceanic-next'
NeoBundle 'tomasr/molokai'
NeoBundle 'fmoralesc/molokayo'
NeoBundle 'nanotech/jellybeans.vim'
NeoBundle 'chriskempson/base16-vim'
NeoBundle 'Yggdroot/indentLine'
NeoBundle 'Raimondi/delimitMate'
NeoBundle 'mxw/vim-jsx'
NeoBundle 'othree/yajs.vim'
" Git helpers
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'jreybert/vimagit'
"NeoBundle 'mhinz/vim-signify'
NeoBundle 'Xuyuanp/nerdtree-git-plugin'
NeoBundle 'LemonBoy/autobahn'
" utils
"NeoBundle 'tpope/vim-repeat'
NeoBundle 'benekastah/neomake'
"NeoBundle 'editorconfig/editorconfig-vim'
NeoBundle 'scrooloose/nerdtree'
"NeoBundle 'AndrewRadev/switch.vim'
NeoBundle 'christoomey/vim-tmux-navigator'
NeoBundle 'tmux-plugins/vim-tmux'
NeoBundle 'tmux-plugins/vim-tmux-focus-events'
NeoBundle 'vim-airline/vim-airline'
NeoBundle 'vim-airline/vim-airline-themes'
NeoBundle 'tpope/vim-surround'
"NeoBundle 'tomtom/tcomment_vim'
NeoBundle 'mattn/emmet-vim'
"NeoBundle 'Chiel92/vim-autoformat'
" NeoBundle 'gorodinskiy/vim-coloresque'
NeoBundle 'ap/vim-css-color'
"NeoBundle 'scrooloose/syntastic'
NeoBundle 'ctrlpvim/ctrlp.vim'
NeoBundle 'mbbill/undotree'
NeoBundle 'scrooloose/nerdcommenter'
"NeoBundle 'KabbAmine/zeavim.vim'
"NeoBundle 'jmcantrell/vim-virtualenv'
NeoBundle 'davidhalter/jedi-vim'
NeoBundle 'majutsushi/tagbar'
"NeoBundle 'myusuf3/numbers.vim'
"NeoBundle 'jbgutierrez/vim-babel'
"NeoBundle 'mattn/webapi-vim'
NeoBundle 'mileszs/ack.vim'

" Shougo
"NeoBundle 'Shougo/neocomplete.vim'
NeoBundle 'Shougo/deoplete.nvim'
NeoBundle 'zchee/deoplete-jedi'
"NeoBundle 'carlitux/deoplete-ternjs', { 'build': { 'mac': 'npm install -g tern', 'unix': 'npm install -g tern' }}
"NeoBundle 'ternjs/tern_for_vim', { 'do': 'npm install' }
"NeoBundle 'zchee/deoplete-clang'
"NeoBundle 'Shougo/unite.vim'
"NeoBundle 'Shougo/unite-outline'
"NeoBundle 'ujihisa/unite-colorscheme'
"NeoBundle 'junkblocker/unite-codesearch'
NeoBundle 'Shougo/vimfiler.vim'
"NeoBundle 'Valloric/YouCompleteMe'
NeoBundle 'Shougo/vimproc.vim', {
    \ 'build' : {
    \     'windows' : 'tools\\update-dll-mingw',
    \     'cygwin' : 'make -f make_cygwin.mak',
    \     'mac' : 'make -f make_mac.mak',
    \     'linux' : 'make',
    \     'unix' : 'gmake',
    \    },
    \ }
NeoBundle 'Shougo/neco-vim'
NeoBundle 'Shougo/neoinclude.vim'
NeoBundleLazy 'ujihisa/neco-look',{'autoload':{'filetypes':['markdown','md']}}

NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'honza/vim-snippets'
" NeoBundle 'junegunn/fzf', { 'dir': '~/.fzf' }
" NeoBundle 'junegunn/fzf.vim'
" NeoBundle 'ashisha/image.vim'
"NeoBundle 'mhinz/vim-sayonara'
"NeoBundle 'mattn/gist-vim', {'depends': 'mattn/webapi-vim'}
NeoBundle 'terryma/vim-multiple-cursors'
"NeoBundle 'rhysd/github-complete.vim'
"NeoBundle 'junegunn/goyo.vim'
"NeoBundle 'https://github.com/danielmiessler/VimBlog'
" NeoBundle 'https://github.com/neovim/node-host'
NeoBundle 'vim-scripts/SyntaxRange'
NeoBundle 'ryanoasis/vim-devicons'

" Lang specifics
"NeoBundle 'rust-lang/rust.vim'
"NeoBundle 'fatih/vim-go'
"NeoBundle 'cespare/vim-toml'

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
"let g:deoplete#enable_at_startup = 1

" Neovim Settings
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
let $TMUX_TUI_ENABLE_CURSOR_SHAPE=1
let $NEOVIM_JS_DEBUG='nvimjs.log'
" Let airline tell me my status
set noshowmode
set noswapfile
filetype on

set splitright
set splitbelow

" relative numbers in normal mode
" regular numbers in insert mode and when not focused
"nnoremap <leader>] :set invrelativenumber<cr>
"autocmd BufLeave,WinLeave,FocusLost * :set norelativenumber 
"autocmd BufEnter,WinEnter,FocusGained * :set relativenumber
"autocmd InsertEnter * :set norelativenumber
"autocmd InsertLeave * :set relativenumber
set number
"set relativenumber

set conceallevel=0
" block select not limited by shortest line
set virtualedit=block
set wildmenu
set laststatus=2
"set colorcolumn=100
set wrap linebreak nolist
set wildmode=full
let g:gitgutter_max_signs = 1000  " default value

" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

" }}}

" System mappings  ----------------------------------------------------------{{{

" No need for ex mode
nnoremap Q <nop>
" exit insert, dd line, enter insert
inoremap <c-d> <esc>ddi
" Navigate between display lines
noremap  <silent> <Up>   gk
noremap  <silent> <Down> gj
noremap  <silent> k gk
noremap  <silent> j gj
noremap  <silent> <Home> g<Home>
noremap  <silent> <End>  g<End>
inoremap <silent> <Home> <C-o>g<Home>
inoremap <silent> <End>  <C-o>g<End>
" copy current files path to clipboard
" Neovim terminal mapping
" terminal 'normal mode'
"tmap <esc> <c-\><c-n><esc><cr>
" ,f to format code, requires formatters: read the docs
"noremap <leader>f :Autoformat<CR>
"noremap <leader>TM :TableModeToggle<CR>
noremap H ^
noremap L g_
noremap J 5j
noremap K 5k
nnoremap ; :
inoremap <c-f> <c-x><c-f>

if $TMUX_RUNNING == 1
    set clipboard=unnamedplus
endif
"vnoremap <C-c> "*y<CR>
"vnoremap y "*y<CR>
"nnoremap Y "*Y<CR>
"let g:multi_cursor_next_key='<C-n>'
"let g:multi_cursor_prev_key='<C-p>'
"let g:multi_cursor_skip_key='<C-x>'
"let g:multi_cursor_quit_key='<Esc>'

" Align blocks of text and keep them selected
nnoremap <leader>d "_d
vnoremap <leader>d "_d

nnoremap <leader>e :call <SID>SynStack()<CR>
function! <SID>SynStack()
if !exists("*synstack")
return
endif
echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

function! s:PlaceholderImgTag(size)
let url = 'http://dummyimage.com/' . a:size . '/000000/555555'
let [width,height] = split(a:size, 'x')
execute "normal a<img src=\"".url."\" width=\"".width."\" height=\"".height."\" />"
endfunction
command! -nargs=1 PlaceholderImgTag call s:PlaceholderImgTag(<f-args>)

" terminal
autocmd BufWinEnter,WinEnter term://* startinsert
autocmd BufLeave term://* stopinsert
tnoremap <ESC> <C-\><C-n><C-w><C-p>

let g:terminal_color_0= "#181818"
let g:terminal_color_1= "#ab4642"
let g:terminal_color_2= "#a1b56c"
let g:terminal_color_3= "#f7ca88"
let g:terminal_color_4= "#7cafc2"
let g:terminal_color_5= "#ba8baf"
let g:terminal_color_6= "#86c1b9"
let g:terminal_color_7= "#d8d8d8"
let g:terminal_color_8= "#585858"
let g:terminal_color_9= "#ab4642"
let g:terminal_color_10="#a1b56c"
let g:terminal_color_11="#f7ca88"
let g:terminal_color_12="#7cafc2"
let g:terminal_color_13="#ba8baf"
let g:terminal_color_14="#86c1b9"
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
autocmd BufRead,BufNewFile *.md,*.rst setlocal spell complete+=kspell
" highlight bad words in red
autocmd BufRead,BufNewFile *.md,*.rst hi SpellBad guibg=#3A3A3A" ctermbg=224
" disable markdown auto-preview. Gets annoying
let g:instant_markdown_autostart = 0
" Keep my termo window open when I navigate away
autocmd TermOpen * set bufhidden=hide

set tabstop=4   
set softtabstop=4
set shiftwidth=4
set expandtab
set hidden
set lazyredraw
autocmd BufRead,BufNewFile *.js,*.html,*.css set tabstop=2
autocmd BufRead,BufNewFile *.js,*.html,*.css set softtabstop=2
autocmd BufRead,BufNewFile *.js,*.html,*.css set shiftwidth=2
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
let python_highlight_all = 1
"let g:gruvbox_termtrans=1
"colorscheme sexy-railscasts-256
"set t_Co=256
"colorscheme OceanicNext
"colorscheme codeschool
"colorscheme sexy-railscasts-256
"let g:solarized_termtrans=1
"let g:solarized_termcolors=256
"colorscheme solarized
"colorscheme elflord
"colorscheme tender
"colorscheme badwolf
"colorscheme gruvbox
"colorscheme onedark
"let g:rehash256 = 1
"colorscheme molokai
"colorscheme Tomorrow-Night
if $VIMCOLOR != 0
    colorscheme kalisi
    set background=light
    highlight CursorLineNr ctermbg=245 ctermfg=250
    highlight Normal ctermbg=231
    highlight MatchParen ctermbg=252 ctermfg=240 cterm=underline
else
    set background=dark
    let base16colorspace=256
    "colorscheme base16-default-dark2
    "colorscheme jellybeans
    "colorscheme hybrid
    "colorscheme monokai
    colorscheme Tomorrow-Night
    "autocmd BufRead,BufNewFile *.js,*.html,*.css colorscheme Tomorrow-Night
    "so $HOME/.config/nvim/custom/base16colors.vim
    "so $HOME/.config/nvim/custom/jellybeanscolors.vim
    so $HOME/.config/nvim/custom/hybridcolors.vim
    "so $HOME/.config/nvim/custom/kalisicolors.vim
    highlight LineNr ctermbg=none ctermfg=241 guibg=#303030 guifg=#606060
    highlight CursorLineNr ctermbg=239 ctermfg=245 guibg=#666666 guifg=#222222 gui=bold
    highlight Normal ctermbg=none ctermfg=251 guibg=#232323 guifg=#d0d0d0
    "highlight Function ctermfg=105
    "highlight String ctermfg=78
    highlight Search cterm=none ctermbg=222 ctermfg=234
    highlight Error ctermbg=203 cterm=none
    highlight VertSplit ctermbg=239 ctermfg=246 guibg=#3d3d3d guifg=#949494
    "highlight MatchParen ctermbg=251 ctermfg=240 cterm=none
    highlight Comment cterm=italic ctermfg=244 guifg=#696969 gui=italic
    highlight Todo cterm=italic ctermbg=114 ctermfg=234 gui=italic guibg=#87d787 guifg=#1c1c1c
    "highlight pythonSelf ctermfg=223
    "highlight pythonClass ctermfg=147
endif

highlight GitGutterAdd ctermbg=none
highlight GitGutterChange ctermbg=none
highlight GitGutterChangeDelete ctermbg=none
highlight GitGutterDelete ctermbg=none
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
highlight ColorColumn ctermbg=239 guibg=#404040
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

function! JavaScriptFold() "{{{
  " syntax region foldBraces start=/{/ end=/}/ transparent fold keepend extend
  setlocal foldmethod=syntax
  setlocal foldlevel=99
  echo "hello"
  syn region foldBraces start=/{/ skip=/\(\/\/.*\)\|\(\/.*\/\)/ end=/}/ transparent fold keepend extend
endfunction "}}}

" function! HTMLFold() "{{{
"   " syn sync fromstart
"   set foldmethod=syntax
"   syn region HTMLFold start=+^<\([^/?!><]*[^/]>\)\&.*\(<\1\|[[:alnum:]]\)$+ end=+^</.*[^-?]>$+ fold transparent keepend extend
"   syn match HTMLCData "<!\[CDATA\[\_.\{-}\]\]>" fold transparent extend
"   syn match HTMLCommentFold "<!--\_.\{-}-->" fold transparent extend
" endfunction "}}}

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

" au FileType html call HTMLFold()
" autocmd FileType html setlocal foldmethod=syntax
autocmd FileType html setlocal fdl=99

" autocmd FileType javascript call JavaScriptFold()
autocmd FileType javascript,html,css,scss,typescript setlocal foldlevel=99
autocmd FileType javascript,typescript,css,scss,json setlocal foldmethod=marker
autocmd FileType javascript,typescript,css,scss,json setlocal foldmarker={,}
autocmd FileType coffee setl foldmethod=indent
" au FileType html nnoremap <buffer> <leader>F zfat
" }}}

" NERDTree ------------------------------------------------------------------{{{

autocmd StdinReadPre * let s:std_in=1
" autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
let NERDTreeShowHidden=1
let g:NERDTreeWinSize=30
let g:NERDTreeAutoDeleteBuffer=1
" NERDTress File highlighting
function! NERDTreeHighlightFile(extension, fg, bg, guifg, guibg)
exec 'autocmd FileType nerdtree highlight ' . a:extension .' ctermbg='. a:bg .' ctermfg='. a:fg .' guibg='. a:guibg .' guifg='. a:guifg
exec 'autocmd FileType nerdtree syn match ' . a:extension .' #^\s\+.*'. a:extension .'$#'
endfunction

call NERDTreeHighlightFile('jade', 'green', 'none', 'green', 'none')
call NERDTreeHighlightFile('md', 'blue', 'none', '#6699CC', 'none')
call NERDTreeHighlightFile('config', 'yellow', 'none', '#d8a235', 'none')
call NERDTreeHighlightFile('conf', 'yellow', 'none', '#d8a235', 'none')
call NERDTreeHighlightFile('json', 'green', 'none', '#d8a235', 'none')
call NERDTreeHighlightFile('html', 'yellow', 'none', '#d8a235', 'none')
call NERDTreeHighlightFile('css', 'cyan', 'none', '#5486C0', 'none')
call NERDTreeHighlightFile('scss', 'cyan', 'none', '#5486C0', 'none')
call NERDTreeHighlightFile('coffee', 'Red', 'none', 'red', 'none')
call NERDTreeHighlightFile('js', 'Red', 'none', '#ffa500', 'none')
call NERDTreeHighlightFile('ts', 'Blue', 'none', '#6699cc', 'none')
call NERDTreeHighlightFile('ds_store', 'Gray', 'none', '#686868', 'none')
call NERDTreeHighlightFile('gitconfig', 'black', 'none', '#686868', 'none')
call NERDTreeHighlightFile('gitignore', 'Gray', 'none', '#7F7F7F', 'none')
"}}}

" Snipppets -----------------------------------------------------------------{{{

" Enable snipMate compatibility feature.
let g:neosnippet#enable_snipmate_compatibility = 1
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)
" Tell Neosnippet about the other snippets
let g:neosnippet#snippets_directory='~/.vim/bundle/neosnippet-snippets/neosnippets, ~/Github/ionic-snippets, ~/.vim/bundle/angular-vim-snippets/snippets'

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
let g:syntastic_loc_list_height = 5
let g:syntastic_enable_signs = 1
let g:syntastic_python_checkers = ["flake8"]

"map <F8> :SyntasticCheck<CR>

" YCM
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

" deoplete
let g:deoplete#enable_at_startup = 1
autocmd CompleteDone * pclose!
nnoremap <F7> :call deoplete#refresh()<CR>
inoremap <F7> <C-o>:call deoplete#refresh()<CR>

" deoplete-jedi
let g:python_host_prog = '/home/alex/.virtualenvs/_nv_py2/bin/python'
let g:python3_host_prog = '/home/alex/.virtualenvs/_nv_py3/bin/python'
"let deoplete#sources#jedi#enable_cache=0

" IndentLine
let g:indentLine_color_term = 236
if $VIMCOLOR != 0
    let g:indentLine_color_term = 250
endif
"let g:indentLine_char = │

" flake8
"autocmd FileType python map <buffer> <F7> :call Flake8()<CR>

" CtrlP
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CTRLP & GREP
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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

" Tagbar
let g:tagbar_sort = 0

"}}}

" Typescript & Javscript omni complete --------------------------------------{{{
  let g:vimjs#casesensistive = 1
  let g:vimjs#smartcomplete = 1
  let g:tsuquyomi_disable_quickfix = 1
  let g:vim_json_syntax_conceal = 0
  " autocmd FileType typescript setlocal completeopt+=menu,preview,noselect
  autocmd FileType typescript setlocal completeopt+=menu,preview
  set completeopt+=menu,preview,noselect
  set splitbelow
  let g:deoplete#enable_debug = 2
" set completeopt+=noselect
" let g:deoplete#omni_patterns = {}
" let g:deoplete#omni_patterns.typescript=[
"               \'[^. \t0-9]\.\w*',
"               \'[^. \t0-9]\->\w*',
"               \'[^. \t0-9]\::\w*',
"               \'\s[A-Z][a-z]',
"               \'^\s*@[A-Z][a-z]'
"               \]
"}}}

" Emmet customization -------------------------------------------------------{{{
" Enable Emmet in all modes
" Remapping <C-y>, just doesn't cut it.
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

" unite ---------------------------------------------------------------------{{{
"
let g:unite_data_directory='~/.nvim/.cache/unite'
let g:unite_source_history_yank_enable=1
let g:unite_prompt='» '
let g:unite_source_rec_async_command =['ag', '--follow', '--nocolor', '--nogroup','--hidden', '-g', '', '--ignore', '.git', '--ignore', '*.png', '--ignore', 'lib']

nnoremap <silent> <c-p> :Unite -auto-resize -start-insert -direction=botright file_rec/neovim2<CR>
nnoremap <silent> <leader>c :Unite -auto-resize -start-insert -direction=botright colorscheme<CR>
nnoremap <silent> <leader>u :Unite neobundle/update<CR>

" Custom mappings for the unite buffer
autocmd FileType unite call s:unite_settings()

function! s:unite_settings() "{{{
  " Enable navigation with control-j and control-k in insert mode
  imap <buffer> <C-j>   <Plug>(unite_select_next_line)
  imap <buffer> <C-k>   <Plug>(unite_select_previous_line)
endfunction "}}}

" Git from unite...ERMERGERD ------------------------------------------------{{{
let g:unite_source_menu_menus = {} " Useful when building interfaces at appropriate places
let g:unite_source_menu_menus.git = {
  \ 'description' : 'Fugitive interface',
  \}
let g:unite_source_menu_menus.git.command_candidates = [
  \[' git status', 'Gstatus'],
  \[' git diff', 'Gvdiff'],
  \[' git commit', 'Gcommit'],
  \[' git stage/add', 'Gwrite'],
  \[' git checkout', 'Gread'],
  \[' git rm', 'Gremove'],
  \[' git cd', 'Gcd'],
  \[' git push', 'exe "Git! push " input("remote/branch: ")'],
  \[' git pull', 'exe "Git! pull " input("remote/branch: ")'],
  \[' git pull rebase', 'exe "Git! pull --rebase " input("branch: ")'],
  \[' git checkout branch', 'exe "Git! checkout " input("branch: ")'],
  \[' git fetch', 'Gfetch'],
  \[' git merge', 'Gmerge'],
  \[' git browse', 'Gbrowse'],
  \[' git head', 'Gedit HEAD^'],
  \[' git parent', 'edit %:h'],
  \[' git log commit buffers', 'Glog --'],
  \[' git log current file', 'Glog -- %'],
  \[' git log last n commits', 'exe "Glog -" input("num: ")'],
  \[' git log first n commits', 'exe "Glog --reverse -" input("num: ")'],
  \[' git log until date', 'exe "Glog --until=" input("day: ")'],
  \[' git log grep commits',  'exe "Glog --grep= " input("string: ")'],
  \[' git log pickaxe',  'exe "Glog -S" input("string: ")'],
  \[' git index', 'exe "Gedit " input("branchname\:filename: ")'],
  \[' git mv', 'exe "Gmove " input("destination: ")'],
  \[' git grep',  'exe "Ggrep " input("string: ")'],
  \[' git prompt', 'exe "Git! " input("command: ")'],
  \] " Append ' --' after log to get commit info commit buffers
nnoremap <silent> <Leader>g :Unite -direction=botright -silent -buffer-name=git -start-insert menu:git<CR>
"}}}
"}}}

" Navigate between vim buffers and tmux panels ------------------------------{{{
let g:tmux_navigator_no_mappings = 1
nnoremap <silent> <C-j> :TmuxNavigateDown<cr>
nnoremap <silent> <C-k> :TmuxNavigateUp<cr>
nnoremap <silent> <C-l> :TmuxNavigateRight<cr>
nnoremap <silent> <C-h> :TmuxNavigateLeft<CR>
nnoremap <silent> <C-;> :TmuxNavigatePrevious<cr>
tmap <C-j> <C-\><C-n>:TmuxNavigateDown<cr>
tmap <C-k> <C-\><C-n>:TmuxNavigateUp<cr>
tmap <C-l> <C-\><C-n>:TmuxNavigateRight<cr>
tmap <C-h> <C-\><C-n>:TmuxNavigateLeft<CR>
tmap <C-;> <C-\><C-n>:TmuxNavigatePrevious<cr>
"}}}

" vim-airline ---------------------------------------------------------------{{{
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#branch#enabled = 1
"set hidden
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tabline#show_tab_nr = 1
let g:airline_powerline_fonts = 1
"let g:airline_theme='tomorrow'
let g:airline_theme='oceanicnext'
"let g:airline_theme='base16_eighties'
"let g:airline_theme='alexline'
cnoreabbrev <expr> x getcmdtype() == ":" && getcmdline() == 'x' ? 'Sayonara' : 'x'
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
"}}}
"
set encoding=utf8
set guifont=UbuntuMonoDerivativePowerline\ Nerd\ Font\ Regular\ 13


" Linting -------------------------------------------------------------------{{{


  function! neomake#makers#ft#javascript#eslint()
      return {
          \ 'args': ['-f', 'compact'],
          \ 'errorformat': '%E%f: line %l\, col %c\, Error - %m,' .
          \ '%W%f: line %l\, col %c\, Warning - %m'
          \ }
  endfunction
  let g:neomake_javascript_enabled_makers = ['eslint']
  " call pylint using the current python (venv or global)
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
  "let g:neomake_python_enabled_makers = ['venvpylint']
  "let g:neomake_python_enabled_makers = ['flake8']
  let g:neomake_java_enabled_makers = []
  autocmd! BufWritePost * Neomake
  function! JscsFix()
      let l:winview = winsaveview()
      % ! jscs -x
      call winrestview(l:winview)
  endfunction
  command JscsFix :call JscsFix()
  noremap <leader>j :JscsFix<CR>
  map <F8> :Neomake<CR>
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
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

"" run python script
nnoremap <F2> :w !/usr/bin/env python <CR>

"" tab navigation
nnoremap <M-PageUp>   <Esc>:tabprevious<CR>
nnoremap <M-PageDown> <Esc>:tabnext<CR>
nnoremap <C-M-PageUp> <Esc>:tabnew<CR>
nnoremap <C-M-PageDown> <Esc>:tabnew<CR>

"" various shortcuts
map <F3> :NERDTreeToggle<CR>
map <C-\> :TagbarToggle<CR>
"map <C-n> :set invnu <CR>
"map <C-p> :PresentingStart<CR>
nnoremap <leader>0 :redraw!<CR>
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
nnoremap <leader>a <Esc>:Ack!<CR>
vnoremap > >gv
vnoremap < <gv
cnoreabbrev spch setlocal spell spelllang=en_us
cnoreabbrev secret r !python3 -c 'import os, binascii; print(binascii.hexlify(os.urandom(24)).decode())'

inoremap <C-h> <Left>
inoremap <C-j> <Down>
inoremap <C-k> <Up>
inoremap <C-l> <Right>

" lololol
nmap ; :
" OLOLOLOLOLOLOLOLO
imap jj <Esc>

nnoremap n nzz
nnoremap ]] ]]zz
nnoremap [[ [[zz
nnoremap ]m ]mzz
nnoremap [m [mzz


" buffer navigation
nnoremap <leader>h :bp<CR>
nnoremap <leader>l :bn<CR>
nnoremap <leader>bq :bp <BAR> bd #<CR>
nnoremap <leader>bl :ls<CR>

" tab navigation
unmap <leader>j
nnoremap <leader>j :tabp<CR>
nnoremap <leader>k :tabn<CR>

nnoremap <leader>f :Ack TODO\\|FIXME\\|NOTE **<CR>

" Various macros
let @d = "oimport ipdb; ipdb.set_trace()\e" " python debug trace
