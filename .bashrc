#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Set `fish` shell as default for Termite
[[ $TERM == "xterm-termite" || $TERM == "xterm-256color" ]] && exec fish
