# Disable greeting text.
set fish_greeting

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

# Defaults for GPG.
set -e SSH_AGENT_PID
set -x SSH_AUTH_SOCK $XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh
set -x GPG_TTY (tty)

gpg-connect-agent updatestartuptty /bye >/dev/null
