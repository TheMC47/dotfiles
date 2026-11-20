#!/bin/sh

[ -f /etc/xprofile ] && . /etc/xprofile
[ -f ~/.xprofile ] && . ~/.xprofile

exec /home/yecinem/.local/bin/xmonad-private > "$HOME/.local/share/xmonad/xmonad.log" 2>&1
