#!/bin/sh

# Determine scale factor for active display.
for port in /sys/class/drm/*
do
    if test -f "$port"/enabled && test "$(cat "$port"/enabled)" = "enabled"
    then
        mode="$(head -n 1 "$port"/modes)"
        if test "$(echo "$mode" | cut -dx -f1)" -ge 2560 && \
           test "$(echo "$mode" | cut -dx -f2)" -ge 1200
        then
            export DISPLAY_SCALE_FACTOR=1.45
            break
        fi
    fi
done

# Set default scale factor if none was already set.
if test -z "$DISPLAY_SCALE_FACTOR"
then
    export DISPLAY_SCALE_FACTOR=1
fi
