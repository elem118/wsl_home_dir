# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/muz/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

eval "$(starship init zsh)"

[ -f "/home/muz/.ghcup/env" ] && . "/home/muz/.ghcup/env" # ghcup-env

alias ll='ls -lah'
export EDITOR=vim
alias v=nvim
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias ls='ls --color=auto'
alias grep='grep --color=auto'
export PAGER=less
export LESSCHARSET=utf-8

export PATH="$PATH:$HOME/bin"
