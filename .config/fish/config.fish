# Disable greeting text.
set fish_greeting

# Include local configuration.
if [ -e $HOME/.config/fish/config.local ]
	source $HOME/.config/fish/config.local
end

# Set default theme.
source $HOME/.config/fish/themes/boxfish.fish

# Custom aliases.
alias dot 'git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

# Set TERM='xterm' for SSH connections.
function ssh -w ssh
	env TERM=xterm ssh $argv
end

# Defaults for GPG.
set -e SSH_AGENT_PID
set -x SSH_AUTH_SOCK $XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh
set -x GPG_TTY (tty)

gpg-connect-agent updatestartuptty /bye >/dev/null
