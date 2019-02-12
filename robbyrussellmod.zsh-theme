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

symb="λ"
#symb="⊢"
#symb="»"

local info="%{$fg_bold[white]%}%n@%m%{$reset_color%}"
local ret_status="%(?:%{$fg_bold[green]%}$symb:%{$fg_bold[red]%}$symb)"
local root_ret_status="%(?:%{$fg_bold[green]%}#:%{$fg_bold[red]%}#)"
PROMPT='$(build_prompt)%{$fg[cyan]%}%~%{$reset_color%}$(git_super_status) %(!.${root_ret_status}.${ret_status})%{$reset_color%} '
PROMPT2='%{$fg[cyan]%}┌─╼ $(build_prompt)%{$fg[cyan]%}%(5~|%-1~/…/%2~|%4~)%{$reset_color%}$(git_super_status)
%{$fg[cyan]%}└╼ %(!.${root_ret_status}.${ret_status})%{$reset_color%} '
# PROMPT='%(!.${root_ret_status}.${ret_status}) %{$fg[cyan]%}%~%{$reset_color%} $(git_super_status) '
RPROMPT='[%T]'

switch_prompts() {
    local tmp=$PROMPT
    PROMPT=$PROMPT2
    PROMPT2=$tmp
}

alias sp=switch_prompts
