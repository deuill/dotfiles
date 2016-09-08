# Disable greeting text.
set fish_greeting

# Include local configuration.
source $HOME/.config/fish/config.local

# Custom theme.
if [ -n $theme ]
   source $HOME/.config/fish/themes/$theme.fish
end

# Custom aliases.
alias sc    'ag --pager "less -R"'
alias trash 'gvfs-trash'
alias dot   'git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

# Custom functions.
function diff-color
	diff -u $argv | colordiff | less -R
end
