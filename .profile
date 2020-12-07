#
# ~/.bash_profile
#

# Some variables.
export GOPATH="$HOME/.go"
export PATH="$PATH:$HOME/.local/bin:$GOPATH/bin"
export QT_QPA_PLATFORMTHEME="qt5ct"
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export EDITOR="emacsclient -a nano"
export RIPGREP_CONFIG_PATH="$HOME/.config/ripgrep/ripgreprc"

# Import specific environment variables for user units.
systemctl --user import-environment PATH
