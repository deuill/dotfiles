# Disable greeting text.
set fish_greeting

# Set default theme.
set theme shellder

# Include local configuration.
if [ -e $HOME/.config/fish/config.local ]
	source $HOME/.config/fish/config.local
end

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

# Start SSH agent if needed.
if test -z "$SSH_ENV"
	setenv SSH_ENV $HOME/.ssh/environment
end

if not __ssh_agent_is_started
	__ssh_agent_start
end