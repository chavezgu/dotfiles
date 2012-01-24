#Vi mode
set -o vi

# Check for an interactive session
[ -z "$PS1" ] && return

PATH=$PATH:$HOME/bin:.

#Aliases
alias ls='ls --color=auto'
alias t='task'
alias chavezgu="ssh chavezgu.com"
alias gultec="ssh gchavez@linux.mty.itesm.mx"
alias svim="sudo vim"

PS1='[\u@\h \t \W][$(task count status:pending)]\$ '

complete -cf sudo
complete -cf man
export BROWSER="firefox"
export EDITOR="vim -f"
