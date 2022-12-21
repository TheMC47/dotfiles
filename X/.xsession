#!/usr/bin/env bash
feh --bg-fill Wallpapers/mountain.png &
xmodmap ~/.Xmodmap &
picom --config ~/picom.conf &
dunst &
pkill xembedsniproxy
redshift-gtk &
nm-applet &
volumeicon &
