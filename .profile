# Shared environment variables.
export GOPATH="$HOME/.go"
export RIPGREP_CONFIG_PATH="$HOME/.config/ripgrep/ripgreprc"

export GTK_THEME=Adwaita:dark
export QT_QPA_PLATFORMTHEME="qt5ct"
export QT_SCALE_FACTOR=1.5

export PATH="$PATH:$HOME/.local/bin:$GOPATH/bin"
export EDITOR="emacsclient -a emacs"

# Import specific environment variables for user units.
systemctl --user import-environment PATH

# Start Sway if no existing session has started.
if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ]
then
    exec sway
fi
