#Vi mode
#set -o vi

# Check for an interactive session
[ -z "$PS1" ] && return

PATH=$PATH:$HOME/bin

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

complete -cf sudo
complete -cf man
export BROWSER="firefox"
export EDITOR="vim -f"
