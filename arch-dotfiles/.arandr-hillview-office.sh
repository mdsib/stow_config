#!/bin/sh
xrandr --output eDP1 --mode 1920x1080 --pos 263x1440 --rotate normal || echo "xrandr: eDP1 error"
xrandr --output HDMI2 --mode 2560x1440 --pos 0x0 --rotate normal --primary || echo "xrandr: HDMI2 error"
xrandr --output DP1 --off --output DP2 --off --output HDMI1 --off --output VIRTUAL1 --off || echo "xrandr: off error"
