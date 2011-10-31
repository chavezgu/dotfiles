#Vi mode
set -o vi

# Check for an interactive session
[ -z "$PS1" ] && return

PATH=$PATH:$HOME/bin:.

alias ls='ls --color=auto'
alias tsk='task'
PS1='[\u@\h \t \W][$(task count status:pending)]\$ '

complete -cf sudo
complete -cf man
export BROWSER="firefox"
export EDITOR="vim -f"
