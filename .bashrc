#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Run fish shell instead of bash.
[ -x /usr/bin/fish ] && exec fish

# Various includes and what-not.
[[ -f ~/.bash_profile ]] && . ~/.bash_profile

# Completion for commands after 'sudo'
complete -cf sudo

# Colorization for various applications
alias ls='ls --color=auto'
alias grep='grep --color=always'
