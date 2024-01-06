# Set system-wide environment from systemd environment.
for conf in "$HOME"/.config/environment.d/*.conf; do set -a; . "$conf"; set +a; done

# Import specific environment variables for user units.
systemctl --user import-environment PATH

# Start Sway if no existing session has started.
if test -z "$DISPLAY" && test "$XDG_VTNR" -eq 1
then
    exec systemd-cat --identifier=sway sway
fi
