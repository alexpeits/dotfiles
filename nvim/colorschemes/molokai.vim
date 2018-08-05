set background=dark
colorscheme molokai

hi! link pythonClass pythonExceptions
highlight IncSearch guifg=#dc9656
highlight Pmenu guibg=#323232
highlight PmenuSel gui=bold guifg=#272727

highlight LineNr ctermbg=none ctermfg=241 guibg=#272727 guifg=#606060
highlight CursorLineNr ctermbg=239 ctermfg=245 guibg=#666666 guifg=#222222 gui=bold
highlight Normal ctermbg=none ctermfg=251 guibg=#212121 guifg=#d0d0d0
highlight Search cterm=none ctermbg=222 ctermfg=234
highlight Error guifg=#e5a5a5 guibg=#602020 gui=bold,underline
highlight VertSplit ctermbg=239 ctermfg=246 guibg=none guifg=#585858
highlight Comment cterm=italic ctermfg=244 guifg=#696969 gui=italic
highlight Todo cterm=italic ctermbg=114 ctermfg=234 gui=italic guibg=#87d787 guifg=#1c1c1c
highlight ColorColumn ctermbg=239 guibg=#383838
highlight Folded ctermbg=238 ctermfg=245 cterm=italic guifg=#666666 guibg=#353535
highlight FoldColumn ctermbg=238 ctermfg=245 guifg=#606060 guibg=#272727

let g:airline_theme='oceanicnext'
