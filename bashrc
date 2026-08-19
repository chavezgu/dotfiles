#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

# aliases
source ~/.aliases

# Completion
complete -cf sudo
complete -cf man

export EDITOR="vim"
export PATH="$HOME/.local/bin:$PATH"
