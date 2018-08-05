set background=dark
colorscheme kalisi
highlight MatchParen gui=reverse
hi! link pythonClass Function
"highlight pythonClass guifg=#ff805f
"highlight Number guifg=#ff805f
highlight pythonSelf ctermfg=216 guifg=#B39380
"highlight pythonSelf ctermfg=216 guifg=#9DB47A
highlight pythonDoctest ctermfg=216 guifg=#94be54
hi! link NonText Normal
"highlight String guifg=#e6db74
highlight LineNr ctermbg=none ctermfg=241 guibg=#292929 guifg=#606060
highlight CursorLineNr ctermbg=239 ctermfg=245 guibg=#666666 guifg=#222222 gui=bold
highlight Normal ctermbg=none ctermfg=251 guibg=#252525 guifg=#d0d0d0
highlight Search cterm=none ctermbg=222 ctermfg=234
highlight Error ctermbg=203 cterm=none
highlight VertSplit ctermbg=239 ctermfg=246 guibg=none guifg=#585858
highlight Comment cterm=italic ctermfg=244 guifg=#696969 gui=italic
highlight Todo cterm=italic ctermbg=114 ctermfg=234 gui=italic guibg=#87d787 guifg=#1c1c1c
highlight ColorColumn ctermbg=239 guibg=#383838
let g:airline_theme='oceanicnext'
