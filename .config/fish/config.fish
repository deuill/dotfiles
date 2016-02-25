# Set global variables
set -x PATH $PATH $HOME/.go/bin
set -x GOPATH $HOME/.go

# Disable greeting text
set fish_greeting

# Custom theme.
set theme 'shellder'
source $HOME/.config/fish/themes/$theme.fish

# Custom aliases
alias sc 'ag --pager "less -R"'
alias emacs 'emacs -mm'
alias trash 'gvfs-trash'
alias cfg 'git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

function ciff
	diff -u $argv | colordiff | less -R
end
