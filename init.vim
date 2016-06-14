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
NeoBundle 'dhruvasagar/vim-table-mode'
NeoBundle 'suan/vim-instant-markdown'
" colorscheme & syntax highlighting
NeoBundle 'mhartington/oceanic-next'
NeoBundle 'tomasr/molokai'
NeoBundle 'fmoralesc/molokayo'
NeoBundle 'nanotech/jellybeans.vim'
NeoBundle 'chriskempson/base16-vim'
NeoBundle 'Yggdroot/indentLine'
NeoBundle 'Raimondi/delimitMate'
" Git helpers
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'jreybert/vimagit'
NeoBundle 'mhinz/vim-signify'
NeoBundle 'Xuyuanp/nerdtree-git-plugin'
NeoBundle 'LemonBoy/autobahn'
" utils
NeoBundle 'tpope/vim-repeat'
NeoBundle 'benekastah/neomake'
NeoBundle 'editorconfig/editorconfig-vim'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'AndrewRadev/switch.vim'
NeoBundle 'christoomey/vim-tmux-navigator'
NeoBundle 'tmux-plugins/vim-tmux'
NeoBundle 'tmux-plugins/vim-tmux-focus-events'
NeoBundle 'vim-airline/vim-airline'
NeoBundle 'vim-airline/vim-airline-themes'
NeoBundle 'tpope/vim-surround'
"NeoBundle 'tomtom/tcomment_vim'
NeoBundle 'mattn/emmet-vim'
NeoBundle 'Chiel92/vim-autoformat'
" NeoBundle 'gorodinskiy/vim-coloresque'
NeoBundle 'ap/vim-css-color'
"NeoBundle 'scrooloose/syntastic'
NeoBundle 'ctrlpvim/ctrlp.vim'
NeoBundle 'mbbill/undotree'
NeoBundle 'scrooloose/nerdcommenter'
NeoBundle 'KabbAmine/zeavim.vim'
NeoBundle 'jmcantrell/vim-virtualenv'
NeoBundle 'davidhalter/jedi-vim'
NeoBundle 'majutsushi/tagbar'

" Shougo
"NeoBundle 'Shougo/neocomplete.vim'
"NeoBundle 'Shougo/deoplete.nvim'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/unite-outline'
NeoBundle 'ujihisa/unite-colorscheme'
NeoBundle 'junkblocker/unite-codesearch'
NeoBundle 'Shougo/vimfiler.vim'
NeoBundle 'Valloric/YouCompleteMe'
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
NeoBundle 'matthewsimo/angular-vim-snippets'

" NeoBundle 'junegunn/fzf', { 'dir': '~/.fzf' }
" NeoBundle 'junegunn/fzf.vim'
" NeoBundle 'ashisha/image.vim'
NeoBundle 'mhinz/vim-sayonara'
NeoBundle 'mattn/gist-vim', {'depends': 'mattn/webapi-vim'}
NeoBundle 'terryma/vim-multiple-cursors'
NeoBundle 'rhysd/github-complete.vim'
NeoBundle 'junegunn/goyo.vim'
NeoBundle 'https://github.com/danielmiessler/VimBlog'
" NeoBundle 'https://github.com/neovim/node-host'
NeoBundle 'vim-scripts/SyntaxRange'
NeoBundle 'ryanoasis/vim-devicons'
call neobundle#end()

" Required:
filetype plugin indent on
filetype plugin on

let pluginsExist=1
NeoBundleCheck
" }}}

if pluginsExist
" System Settings  ----------------------------------------------------------{{{

let $NVIM_TUI_ENABLE_TRUE_COLOR=1

let g:python_host_prog = '/usr/bin/python'
"let g:deoplete#enable_at_startup = 1


" Neovim Settings
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
let $NEOVIM_JS_DEBUG='nvimjs.log'
" Let airline tell me my status
set noshowmode
set noswapfile
filetype on

" relative numbers in normal mode
" regular numbers in insert mode and when not focused
nnoremap <leader>] :set invrelativenumber<cr>
autocmd BufLeave,WinLeave,FocusLost * :set norelativenumber 
autocmd BufEnter,WinEnter,FocusGained * :set relativenumber
autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber
set number
set relativenumber

set conceallevel=0
" block select not limited by shortest line
set virtualedit=
set wildmenu
set laststatus=2
"set colorcolumn=100
set wrap linebreak nolist
set wildmode=full
let g:gitgutter_max_signs = 1000  " default value
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
noremap <leader>f :Autoformat<CR>
"noremap <leader>TM :TableModeToggle<CR>
noremap H ^
noremap L g_
noremap J 5j
noremap K 5k
" this is the best, let me tell you why
" how annoying is that everytime you want to do something in vim
" you have to do shift-; to get :, can't we just do ;?
" Plus what does ; do anyways??
" if you do have a plugin that needs ;, you can just wap the mapping
" nnoremap : ;
" give it a try and you will like it
nnoremap ; :
inoremap <c-f> <c-x><c-f>
" Copy to osx clipboard
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
"}}}"

" Themes, Commands, etc  ----------------------------------------------------{{{
" Theme
syntax enable
set background=dark
" set background=light
" no need to fold things in markdown all the time
let g:vim_markdown_folding_disabled = 1
" turn on spelling for markdown files
autocmd BufRead,BufNewFile *.md setlocal spell complete+=kspell
" highlight bad words in red
autocmd BufRead,BufNewFile *.md hi SpellBad guibg=#ff2929 guifg=#ffffff" ctermbg=224
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

set updatetime=100

set nu
set background=dark
let python_highlight_all = 1
"let g:gruvbox_termtrans=1
"colorscheme sexy-railscasts-256
"set t_Co=256
colorscheme OceanicNext2
"colorscheme sexy-railscasts-256
"let g:solarized_termtrans=1
"let g:solarized_termcolors=256
"colorscheme solarized
"colorscheme elflord
"colorscheme tender
"colorscheme gruvbox
"colorscheme onedark
"let g:rehash256 = 1
"colorscheme molokai
"colorscheme Tomorrow-Night
highlight LineNr ctermbg=none ctermfg=241
highlight CursorLineNr ctermbg=239 ctermfg=245
highlight Normal ctermbg=none ctermfg=251
"highlight Function ctermfg=105
"highlight String ctermfg=78
highlight Search cterm=none ctermbg=222 ctermfg=234
highlight Error ctermbg=203
highlight VertSplit ctermbg=239 ctermfg=246
highlight MatchParen ctermbg=251 ctermfg=240
highlight Comment cterm=italic ctermfg=243
highlight Todo cterm=italic ctermbg=114 ctermfg=234
highlight pythonSelf ctermfg=223

" Tomorrow-Night
"highlight Number ctermfg=167
"highlight pythonSelf ctermfg=210
"highlight pythonDoctest ctermfg=74
"highlight pythonAsync ctermfg=209
"highlight pythonEscape ctermfg=167
highlight pythonClass ctermfg=3
"highlight Folded ctermbg=238

highlight GitGutterAdd ctermbg=none
highlight GitGutterChange ctermbg=none
highlight GitGutterChangeDelete ctermbg=none
highlight GitGutterDelete ctermbg=none
let g:gitgutter_map_keys = 0

" highlight if line exceeds specified amount
highlight ColorColumn ctermbg=238
au BufRead,BufNewFile *.py call matchadd('ColorColumn', '\%79v', 100) "set column nr

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
let g:NERDTreeWinSize=45
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
let g:syntastic_loc_list_height = 5
let g:syntastic_enable_signs = 1
let g:syntastic_python_checkers = ["flake8"]

"map <F8> :SyntasticCheck<CR>

" YCM
nnoremap <leader>jd :YcmCompleter GoTo<CR>
nnoremap <leader>jr :YcmCompleter GoToReferences<CR>
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_global_ycm_extra_conf = '~/.vim/confs/.ycm_extra_conf.py'

" jedi-vim
let g:jedi#completions_enabled = 0
let g:jedi#popup_on_dot = 0
let g:jedi#smart_auto_mappings = 0

" IndentLine
let g:indentLine_color_term = 239
"let g:indentLine_char = │

" flake8
autocmd FileType python map <buffer> <F7> :call Flake8()<CR>

" CtrlP
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

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
  autocmd FileType html,css EmmetInstall
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
set hidden
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tabline#show_tab_nr = 1
let g:airline_powerline_fonts = 1
"let g:airline_theme='molokai'
let g:airline_theme='oceanicnext'
"let g:airline_theme='alexline'
cnoreabbrev <expr> x getcmdtype() == ":" && getcmdline() == 'x' ? 'Sayonara' : 'x'
"tmap <leader>x <c-\><c-n>:bp! <BAR> bd! #<CR>
nmap <leader>t :term<cr>
nmap <leader>, :bnext<CR>
tmap <leader>, <C-\><C-n>:bnext<cr>
nmap <leader>. :bprevious<CR>
tmap <leader>. <C-\><C-n>:bprevious<CR>
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

  let g:neomake_python_enabled_makers = ['venvpylint']
  "let g:neomake_python_enabled_makers = ['flake8']
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
"set splitbelow
"set splitright

"" run python script
nnoremap <F2> :w !python <CR>

"" tab navigation
nnoremap <M-PageUp>   <Esc>:tabprevious<CR>
nnoremap <M-PageDown> <Esc>:tabnext<CR>
nnoremap <C-M-PageUp> <Esc>:tabnew<CR>
nnoremap <C-M-PageDown> <Esc>:tabnew<CR>

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
nnoremap <leader>[ :noh<CR>
nnoremap <leader>= :cn<CR>
nnoremap <leader>- :cp<CR>
nnoremap <leader>+ :copen<CR>
nnoremap <leader>_ :cclose<CR>
vnoremap > >gv
vnoremap < <gv

" lololol
nmap ; :
nnoremap n nzz


" buffer navigation
nnoremap <leader>h :bp<CR>
nnoremap <leader>l :bn<CR>
nnoremap <leader>bq :bp <BAR> bd #<CR>
nnoremap <leader>bl :ls<CR>

" Various macros
let @d = "oimport ipdb; ipdb.set_trace()\e" " python debug trace
