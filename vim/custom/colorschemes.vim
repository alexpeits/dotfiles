if $VIMCOLOR == "light"
    colorscheme kalisi
    set background=light
    hi! link pythonClass Number
    highlight CursorLineNr ctermbg=245 ctermfg=250
    highlight Normal ctermbg=231
    highlight MatchParen ctermbg=252 ctermfg=240 cterm=underline gui=reverse
    highlight pythonSelf ctermfg=216 guifg=#A56845
    highlight pythonDoctest ctermfg=216 guifg=#A850B3
    highlight ColorColumn ctermbg=239 guibg=#aaaaaa
    let g:airline_theme='solarized'
elseif $VIMCOLOR == "solarized"
    set background=dark
    let base16colorspace=256
    colorscheme flattened_dark
    highlight MatchParen gui=reverse
    highlight Search cterm=none ctermbg=222 ctermfg=234
    highlight Error ctermbg=203 cterm=none
    "highlight Normal ctermbg=none guibg=none
    "highlight VertSplit ctermbg=239 ctermfg=246 guibg=#3d3d3d guifg=#949494
    highlight VertSplit ctermbg=239 ctermfg=246 guibg=none guifg=#585858
    "highlight MatchParen ctermbg=251 ctermfg=240 cterm=none
    highlight Comment cterm=italic ctermfg=244 guifg=#696969 gui=italic
    highlight Todo cterm=italic ctermbg=114 ctermfg=234 gui=italic guibg=#87d787 guifg=#1c1c1c
    "highlight pythonSelf ctermfg=223
    "highlight pythonClass ctermfg=147
    highlight ColorColumn ctermbg=239 guibg=#383838
    let g:airline_theme='solarized'
elseif $VIMCOLOR == "oceanic"
    set background=dark
    colorscheme OceanicNext
    "colorscheme OceanicNext
    "highlight Error ctermbg=203 cterm=none
    highlight VertSplit ctermbg=239 ctermfg=246 guibg=none guifg=#585858
    highlight Comment cterm=italic gui=italic
    let g:airline_theme='oceanicnext'
elseif $VIMCOLOR == "base16"
    set background=dark
    colorscheme base16-tomorrow-night
    "highlight Error ctermbg=203 cterm=none
    highlight VertSplit ctermbg=239 ctermfg=246 guibg=none guifg=#585858
    highlight Comment cterm=italic gui=italic
    highlight pythonSelf ctermfg=216 guifg=#C9A17D
	highlight Visual ctermbg=240 guibg=#434343
    highlight MatchParen guibg=#3A3A3A guifg=#D7AF87 gui=underline
    highlight WildMenu ctermbg=2 ctermfg=236 cterm=bold guifg=#212121 guibg=#aaaaaa gui=bold
    highlight Search guifg=#282828
    let g:airline_theme='oceanicnext'
elseif $VIMCOLOR == "monokai"
    set background=dark
    colorscheme molokai
    "colorscheme OceanicNext
    "highlight Error ctermbg=203 cterm=none
    highlight VertSplit ctermbg=239 ctermfg=246 guibg=none guifg=#585858
    highlight Comment cterm=italic ctermfg=244 guifg=#696969 gui=italic
    hi PreProc ctermfg=81 ctermbg=NONE cterm=NONE guifg=#66d9ef guibg=NONE
    hi pythonClass ctermfg=81 ctermbg=NONE cterm=NONE guifg=#66d9ef guibg=NONE
    highlight Folded ctermbg=238 ctermfg=245 cterm=italic guifg=#666666 guibg=#353535
    highlight LineNr ctermbg=none ctermfg=241 guibg=#272727 guifg=#606060
    highlight CursorLineNr ctermbg=239 ctermfg=245 guibg=#666666 guifg=#222222 gui=bold
    highlight Normal ctermbg=none ctermfg=251 guibg=#212121 guifg=#d0d0d0
    highlight Pmenu ctermbg=none ctermfg=251 guibg=#363636
    highlight Error guifg=#e5a5a5 guibg=#602020
    let g:airline_theme='oceanicnext'
else
    set background=dark
    let base16colorspace=256
    "colorscheme base16-default-dark2
    "colorscheme jellybeans
    "colorscheme hybrid
    "colorscheme Tomorrow-Night
    colorscheme spacemacs-theme
    highlight MatchParen gui=reverse
    highlight Error guifg=#e5a5a5 guibg=#602020
    highlight Folded ctermbg=238 ctermfg=245 cterm=italic guifg=#666666 guibg=#353535
    highlight pythonClass guifg=#CA2B7F gui=bold
    highlight pythonDecorator guifg=#D7005F gui=bold
    highlight pythonDecoratorName guifg=#D7005F gui=bold
    highlight pythonFunction gui=bold guifg=#bc6ec5
    highlight pythonSelf guifg=#82C0F5
    highlight CursorLine guibg=#323244
    "autocmd BufRead,BufNewFile *.js,*.html,*.css colorscheme Tomorrow-Night
    "so $HOME/.config/nvim/custom/base16colors.vim
    "so $HOME/.config/nvim/custom/jellybeanscolors.vim
    "so $HOME/.config/nvim/custom/hybridcolors.vim
    "so $HOME/.config/nvim/custom/kalisicolors.vim
    highlight LineNr ctermbg=none ctermfg=241 guibg=#272727 guifg=#606060
    highlight CursorLineNr ctermbg=239 ctermfg=245 guibg=#666666 guifg=#222222 gui=bold
    highlight Normal ctermbg=none ctermfg=251 guibg=#212121 guifg=#d0d0d0
    "highlight Function ctermfg=105
    "highlight String ctermfg=78
    highlight Search cterm=none ctermbg=222 ctermfg=234
    highlight Error ctermbg=203 cterm=none
    "highlight VertSplit ctermbg=239 ctermfg=246 guibg=#3d3d3d guifg=#949494
    highlight VertSplit ctermbg=239 ctermfg=246 guibg=none guifg=#585858
    "highlight MatchParen ctermbg=251 ctermfg=240 cterm=none
    highlight Comment cterm=italic ctermfg=244 guifg=#696969 gui=italic
    highlight Todo cterm=italic ctermbg=114 ctermfg=234 gui=italic guibg=#87d787 guifg=#1c1c1c
    "highlight pythonSelf ctermfg=223
    "highlight pythonClass ctermfg=147
    highlight ColorColumn ctermbg=239 guibg=#383838
    let g:airline_theme='spacemacs'
endif

