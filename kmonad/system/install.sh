#!/usr/bin/env bash
# Install the kmonad udev rule and systemd template unit into /etc, then
# re-trigger udev so a kmonad instance starts for each matching keyboard.
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)

sudo install -Dm644 "$here/etc/udev/rules.d/70-kmonad.rules" /etc/udev/rules.d/70-kmonad.rules
sudo install -Dm644 "$here/etc/systemd/system/global-kmonad@.service" /etc/systemd/system/global-kmonad@.service

sudo systemctl daemon-reload
sudo udevadm control --reload

# Stop running instances so devices that no longer match don't keep one.
sudo systemctl stop 'global-kmonad@*.service' || true
sudo udevadm trigger --action=add --subsystem-match=input

sleep 1
systemctl list-units --no-legend 'global-kmonad@*'
