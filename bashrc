#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source ~/.aliases
PS1='[\u@\h \W]\$ '

# Completion
complete -cf sudo
complete -cf man

export EDITOR="vim"
