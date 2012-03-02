#Vi mode
set -o vi

# Check for an interactive session
[ -z "$PS1" ] && return

#enviroment variables
PATH=$PATH:$HOME/bin:.
CDPATH=.:~:~/Projects:/etc

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
export HISTCONTROL=ignoredups #ignore duplicates in bash history
export HISTSIZE=2000

#ssh agent
eval $(keychain --eval --agents ssh -Q --quiet id_rsa)

#functions
function mkdircd () { 
    mkdir -p "$@" && eval cd "\"\$$#\"";
}

upvimplugins() {
    cd ~/dotfiles; 
    git submodule foreach git pull origin master;
    cd -;
}
