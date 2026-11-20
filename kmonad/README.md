# kmonad

- `.config/kmonad/config.kbd`: the layout, stowed into `$HOME` (`stow kmonad`).
- `system/`: a udev rule and a systemd template unit, installed into `/etc`
  with `system/install.sh` (stow ignores this directory).

## How it starts

There is no user-session launcher. When a keyboard shows up, the udev rule
`70-kmonad.rules` adds `SYSTEMD_WANTS=global-kmonad@<devnode>.service` to it,
and systemd starts one root kmonad per keyboard:

```
/usr/bin/kmonad -i 'device-file "/dev/input/eventN"' ~/.config/kmonad/config.kbd
```

The `-i` flag overrides the `input` line in `config.kbd`, so one config serves
every keyboard. Unplugging the keyboard stops its instance.

## Setting up a new machine

1. Install kmonad (`pacman -S kmonad`, or the AUR / upstream release).
2. Allow uinput: `sudo modprobe uinput`, and see the kmonad FAQ below for the
   uinput group/permissions rule if kmonad can't open `/dev/uinput`.
3. `stow kmonad`
4. `kmonad/system/install.sh`
5. Check: `systemctl list-units 'global-kmonad@*'` should show one unit per
   real keyboard.

The service has the config path `/home/yecinem/...` hardcoded; change it in
`system/etc/systemd/system/global-kmonad@.service` if the user differs.

## Adding or excluding a device

Find the event node and what udev knows about it:

```sh
ls -l /dev/input/by-id/ /dev/input/by-path/   # name -> eventN
udevadm info /dev/input/eventN                # ID_INPUT_KEYBOARD, ID_VENDOR_ID, ...
udevadm info -a /dev/input/eventN             # ATTRS{} of every parent device
udevadm test /sys/class/input/eventN          # dry-run the rules against it
```

Then edit `70-kmonad.rules` (match on `ENV{...}` or `ATTRS{...}` from the
output above) and rerun `system/install.sh`.

## References

- udev rule syntax (match keys, `ATTRS`, `ENV`, `PROGRAM`, `$result`):
  https://www.freedesktop.org/software/systemd/man/latest/udev.html
- `SYSTEMD_WANTS` and starting a template unit per device:
  https://www.freedesktop.org/software/systemd/man/latest/systemd.device.html
- Arch Wiki, udev (writing rules, `udevadm info`/`test`, debugging):
  https://wiki.archlinux.org/title/Udev
- Arch Wiki, KMonad: https://wiki.archlinux.org/title/KMonad
- kmonad FAQ (uinput permissions, finding the device, one config for several
  keyboards): https://github.com/kmonad/kmonad/blob/master/doc/faq.md
- Upstream kmonad startup examples (systemd unit, not per-device):
  https://github.com/kmonad/kmonad/tree/master/startup
