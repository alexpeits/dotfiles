# Path to your oh-my-zsh installation.
  export ZSH=/home/alex/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussellmod"

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

plugins=(git mercurial nix)

source $ZSH/oh-my-zsh.sh
#
# Example aliases
plugins=(… zsh-completions)
fpath+=~/.zfunc
autoload -U compinit && compinit
autoload -U +X bashcompinit && bashcompinit
# only correct commands, not arguments
setopt nocorrectall; setopt correct
#source $HOME/.oh-my-zsh/plugins/git-prompt/git-prompt.plugin.zsh
source $HOME/git_prompt.zsh

eval "$(stack --bash-completion-script stack)"

# prefix prompt when in nix-shell
prompt_nix_shell_setup

export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_EDITOR=vim

export EDITOR=vim

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(echo $history[$HISTCMD] | sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

alias u='cd ..'
alias uu='cd ../..'
alias uuu='cd ../../..'
alias uuuu='cd ../../../..'
alias detach='udisksctl power-off -b'
alias emacs="emacs -nw"
alias tmux="TERM=xterm-256color tmux"
alias tmux-simple="TERM=xterm-256color tmux -f /home/alex/.tmux-simple.conf"
alias rmpyc='find . | grep -E "(__pycache__|\.pyc|\.pyo$)" | xargs rm -rf'
#some haskell aliases
alias stack-test-watch="stack build --file-watch --test --test-arguments '--rerun --rerun-all-on-success'"
alias ghcid-test="ghcid --command='stack exec ghci --test -- -isrc -itest test/Spec.hs'  --test $':main --rerun'"

# python
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/devel
source /usr/local/bin/virtualenvwrapper.sh

export PYENV_ROOT="$HOME/.pyenv"

# java
export JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64"

export LC_CTYPE=en_US.UTF-8

# GBA dev variables
export DEVKITPRO=/opt/devkitpro
export DEVKITARM=/opt/devkitpro/devkitARM
export PATH=$PATH:/opt/devkitpro/devkitARM/bin

count() {
    grep -crI --exclude-dir=.git $1 * | sed '/:0$/d'
}

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

# some ledger stuff
statement() {
    CMD="hledger register assets:bank:hsbc -p"
    if [ $# -eq 0 ]; then
        eval "$CMD thismonth"
    else
        eval "$CMD $1"
    fi
}

lambdabot() {
    (cd ~/sources/lambdabot-sb && cabal exec lambdabot)
}

stack-watch() {
    args="$@"
    find src/ -type f | entr -r -c stack build --exec "$args"
}

export PATH="$PYENV_ROOT/bin:$HOME/bin:$HOME/.local/bin:$HOME/.cabal/bin:$HOME/.cargo/bin:$PATH"
export LC_CTYPE=en_US.UTF-8

if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
