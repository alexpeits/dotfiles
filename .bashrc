# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

. ~/.git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_EDITOR=vim

if [ "$color_prompt" = yes ]; then
    #PS1='${debian_chroot:+($debian_chroot)}\[\033[01;33m\]\u@\h\[\033[00m\] \[\033[01;34m\]\w \[\033[01;31m\]$(__git_ps1 "(%s) ")\[\033[01;34m\]\$\[\033[00m\] '
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;33m\][\u]\[\033[00m\] \[\033[01;34m\]\w \[\033[01;31m\]$(__git_ps1 "(%s) ")\[\033[01;34m\]\$\[\033[00m\] '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -lA'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

alias publicip='curl ipv4.icanhazip.com'
alias rpi='ssh -p 2222 pi@192.168.1.56'
alias pylib="cd /usr/lib/python2.7/"
alias pylib_extern="cd /usr/local/lib/python2.7/dist-packages/"
alias u='cd ..'
alias uu='cd ../..'
alias uuu='cd ../../..'
alias uuuu='cd ../../../..'
alias uuuuu='cd ../../../../..'
alias detach='udisksctl power-off -b'
alias nv=nvim
alias nvi=nvim
alias tmux="TERM=screen-256color-bce tmux"

export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/devel
source /usr/local/bin/virtualenvwrapper.sh

export EDITOR=vim

# colorize man pages with less
man() {
    env LESS_TERMCAP_mb=$'\E[01;31m' \
    LESS_TERMCAP_md=$'\E[00;31m' \
    LESS_TERMCAP_me=$'\E[0m' \
    LESS_TERMCAP_se=$'\E[0m' \
    LESS_TERMCAP_so=$'\E[7;49;37m' \
    LESS_TERMCAP_ue=$'\E[0m' \
    LESS_TERMCAP_us=$'\E[04;35;5;146m' \
    man "$@"
}

# Grep for pattern recursively,
# starting from current path down
#search() {

#    grep -rI $1 *
#
#}

# Grep for pattern recursively,
# starting from current path down.
# Accepts multiple arguments.
#search() {
#
#    #FOO=${@:1:1}
#    PARAMS=""
#    for i in "$@"; do
#        PARAMS="$PARAMS -e $i"
#    done
#    grep -nrI $PARAMS *
#
#}
# Grep for pattern recursively,
# starting from current path down.
# Accepts multiple arguments and
# an optional argument to surround
# lines with results (search [-<num_of_lines>] ...)
search() {

    SURR=${@:1:1}
    if [[ $SURR =~ -[0-9]* ]] ; then
        PARAMS=$SURR
    else
        PARAMS="-e $1"
    fi
    for i in "${@:2}"; do
        PARAMS="$PARAMS -e $i"
    done
    #grep -nrI $PARAMS *k
    grep -nrI $PARAMS *

}

# same as search, but open all files in vim and highlight occurences
visearch() {

    search $1 | awk -F":" '{print $1}' | uniq | cat | xargs bash -c '</dev/tty nvim -c /'$1' $@' ignoreme

}

# Count appearances of a word in a directory
# (recursively). Excludes .git folder
count() {

    grep -crI --exclude-dir=.git $1 * | sed '/:0$/d'

}
export LC_CTYPE=en_US.UTF-8

TERM=xterm-256color
. ~/.git-completion.bash
#source $HOME/.vim/gruvbox_256palette.sh
#BASE16_SHELL="$HOME/sources/base16-shell/base16-default.dark.sh"
#[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

export DNS1=10.10.10.11
export DNS2=10.10.11.11
