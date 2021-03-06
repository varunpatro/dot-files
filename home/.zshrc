# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# HYPHEN_INSENSITIVE="true"

COMPLETION_WAITING_DOTS="true"

plugins=(git z zsh-autosuggestions osx)

source $ZSH/oh-my-zsh.sh

# User configuration
export MANPATH="/usr/local/man:$MANPATH"

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='emacsclient -t -c -a ""'                  # $EDITOR should open in terminal
else
    export EDITOR='emacsclient -t -c -a ""'
    export VISUAL='emacsclient -t -c -a ""'         # $VISUAL opens in GUI with non-daemon as alternate
fi

man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
            man "$@"
}

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.

alias e=$EDITOR
alias glo="git log --oneline --decorate --graph --all"
if [[ -n $(command -v hub) ]]; then
    alias git="hub"
fi

alias ll="ls -lath"

# Source Profile
source ~/.profile

# Custom
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
c2p () {
	file=$1
	folder=${1%.*}
	unzip -d $folder $file
	pushd $folder
	convert *.png ../$folder.pdf
	popd
}
cj2p () {
	file=$1
	folder=${1%.*}
	unzip -d $folder $file
	pushd $folder
	convert *.jpg ../$folder.pdf
	popd
}
