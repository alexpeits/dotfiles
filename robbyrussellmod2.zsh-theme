# vim: set ft=zsh:
prompt_status() {
  local symbols
  symbols=()
  [[ $(jobs -l | wc -l) -gt 0 ]] && symbols+="%{%F{white}%}⚙"

  [[ -n "$symbols" ]] && echo -n "$symbols "
}

build_prompt() {
  prompt_status
}

ZSH_THEME_HG_PROMPT_PREFIX="%{$fg_bold[magenta]%}hg:(%{$fg[red]%}"
ZSH_THEME_HG_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_HG_PROMPT_DIRTY="%{$fg[magenta]%}) %{$fg[yellow]%}✗%{$reset_color%}"
ZSH_THEME_HG_PROMPT_CLEAN="%{$fg[magenta]%})"

#local ret_status="%(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}➜ )"
local ret_status="%(?:%{$FG[105]%}»:%{$FG[168]%}»)%{$reset_color%} "
local root_ret_status="%(?:%{$FG[105]%}#:%{$FG[168]%}#)%{$reset_color%} "
PROMPT='$(build_prompt)%{$fg[cyan]%}%~%{$reset_color%}$(git_super_status)$(hg_prompt_info) %(!.${root_ret_status}.${ret_status})'
