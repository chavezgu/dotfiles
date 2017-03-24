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

export BROWSER="firefox"
export GOPATH=$HOME/projects/go

export PATH="/home/egachav/bin/:$PATH:$GOPATH/bin"

# Python config
export WORKON_HOME=~/.virtualenvs
source /usr/bin/virtualenvwrapper.sh
export GTAGSLABEL=pygments
