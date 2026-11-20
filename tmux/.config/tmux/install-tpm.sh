#!/bin/sh
# Install tpm and every @plugin listed in tmux.conf.
# Called by tmux.conf when tpm is missing, and safe to run by hand during setup.
plugins="$HOME/.local/share/tmux/plugins"

say() {
  echo "$1"
  tmux display-message -d 4000 "$1" 2>/dev/null || true
}

[ -d "$plugins/tpm" ] && exit 0

say "tmux: installing tpm and plugins..."
if ! git clone -q https://github.com/tmux-plugins/tpm "$plugins/tpm"; then
  say "tmux: tpm install failed (offline?), will retry on next tmux start"
  exit 1
fi
# install_plugins asks the running tmux server for the install path, and a
# server started before this config was loaded would still have tpm's default
# (~/.config/tmux/plugins, i.e. inside the dotfiles repo). Set it explicitly.
tmux set-environment -g TMUX_PLUGIN_MANAGER_PATH "$plugins/" 2>/dev/null || true
if ! "$plugins/tpm/bin/install_plugins" >/dev/null; then
  # Remove tpm again so the next tmux start retries the whole install.
  rm -rf "$plugins/tpm"
  say "tmux: plugin install failed (offline?), will retry on next tmux start"
  exit 1
fi
tmux source-file "$HOME/.config/tmux/tmux.conf" 2>/dev/null || true
say "tmux: tpm and plugins installed"
