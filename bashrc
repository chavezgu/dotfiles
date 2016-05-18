#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

source ~/.aliases

export BROWSER="firefox"
export PATH="/home/egachav/bin/:$PATH"

# Completion
complete -cf sudo
complete -cf man
