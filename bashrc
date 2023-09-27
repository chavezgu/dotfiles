#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# PS1='[\u@\h \W]\$ '
PS1='$ '

source ~/.aliases

# Completion
complete -cf sudo
complete -cf man

export EDITOR="vim"
