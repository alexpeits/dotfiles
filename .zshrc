# Path to your oh-my-zsh installation.
  export ZSH=/home/alex/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussellmod"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git mercurial)

# User configuration

# export PATH="/usr/bin:/bin:/usr/sbin:/sbin:$PATH"
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
plugins=(… zsh-completions)
autoload -U compinit && compinit
autoload -U +X bashcompinit && bashcompinit
#source $HOME/.oh-my-zsh/plugins/git-prompt/git-prompt.plugin.zsh
source $HOME/git_prompt.zsh

eval "$(stack --bash-completion-script stack)"

export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_EDITOR=nvim

alias ll='ls -lA'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
#alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(echo $history[$HISTCMD] | sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

alias publicip='curl ipv4.icanhazip.com'
alias pylib="cd /usr/lib/python2.7/"
alias pylib3="cd /usr/lib/python3.5/"
alias pylib_extern="cd /usr/local/lib/python2.7/dist-packages/"
alias u='cd ..'
alias uu='cd ../..'
alias uuu='cd ../../..'
alias uuuu='cd ../../../..'
alias uuuuu='cd ../../../../..'
alias detach='udisksctl power-off -b'
alias nv=nvim
alias nvi=nvim
alias emacs="emacs -nw"
alias gemacs="env HOME=/home/alex/alt-emacs /usr/bin/emacs"
alias emacs-classic="env HOME=/home/alex/alt-emacs /usr/bin/emacs -nw"
alias nview="nvim -R"
alias tmux="TERM=xterm-256color tmux"
alias tmux-simple="TERM=xterm-256color tmux -f /home/alex/.tmux-simple.conf"
alias redis="rm dump.rdb; redis-server; rm dump.rdb"
alias rmpyc='find . | grep -E "(__pycache__|\.pyc|\.pyo$)" | xargs rm -rf'
alias ipy="python -c 'import IPython; IPython.terminal.ipapp.launch_new_instance()'"
#alias browser="google-chrome"

export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/devel
source /usr/local/bin/virtualenvwrapper.sh

export EDITOR=nvim
export PYTHONCURRENT="2"
export PYTHONPATH=$HOME/.python_local_path:$PYTHONPATH

# GO
#export PATH=$PATH:/usr/local/go/bin
#export GOPATH=$HOME/go_projects

export JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64"

export PATH=$PATH:$HOME/bin:$HOME/.local/bin:$HOME/.cabal/bin

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

#nsearch() {

    #search $@ | awk '{ print NR ":" $0 }'

#}

#vopen() {

    ##read list <<< EOF
    ##echo $list
    ##nsearch $@ | awk -F":" '/^'$1'/ { print $2 " +" $3 }' | xargs bash -c '</dev/tty nvim $@' ignoreme
    ##cat $* | awk -F":" '/^'$1'/ { print $2 " +" $3 }'

#}

# same as search, but open all files in vim and show results in quickfix
visearch() {

    search $1 | awk -F":" '{print $1}' | uniq | cat | xargs bash -c '</dev/tty nvim -c "vimgrep /"'$1'"/ ##" -c copen $@' ignoreme

}

# same as above, but open all files in vim and highlight occurences
visearch2() {

    search $1 | awk -F":" '{print $1}' | uniq | cat | xargs bash -c '</dev/tty nvim -c /'$1' $@' ignoreme

}

# Count appearances of a word in a directory
# (recursively). Excludes .git folder
count() {

    grep -crI --exclude-dir=.git $1 * | sed '/:0$/d'

}
export LC_CTYPE=en_US.UTF-8

# GBA dev variables
export DEVKITPRO=/opt/devkitpro
export DEVKITARM=/opt/devkitpro/devkitARM
export PATH=$PATH:/opt/devkitpro/devkitARM/bin

rbinit() {
    export PATH=$PATH:$HOME/.rbenv/bin
    eval "$(rbenv init -)"
}

nvminit() {
    unset -f npm
    export NVM_DIR="/home/alex/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
}

npm() {
    if [ -x npm ]; then
        command npm $*
    else
        nvminit && command npm $*
    fi
}
