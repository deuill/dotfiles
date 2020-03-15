#
# ~/.bash_profile
#

# Some variables.
export GOPATH="$HOME/.go"
export PATH="$PATH:$HOME/.local/bin:$GOPATH/bin"
export QT_QPA_PLATFORMTHEME="qt5ct"
export EDITOR="emacsclient -a nano"

# Import specific environment variables for user units.
systemctl --user import-environment PATH
