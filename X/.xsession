#!/usr/bin/env bash
feh --bg-scale Wallpapers/wolf.jpg &
xmodmap ~/.Xmodmap &
picom --config ~/picom.conf &
dunst &
pkill xembedsniproxy
redshift-gtk &
nm-applet &
volumeicon &
