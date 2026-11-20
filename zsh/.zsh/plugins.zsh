# Minimal plugin loader: clone on first use, source, update on demand.
# Plugins live outside the dotfiles repo; `zsh-plugins-update` pulls them all.

ZSH_PLUGIN_DIR=${XDG_DATA_HOME:-$HOME/.local/share}/zsh/plugins

zsh_plugins=(
  zsh-users/zsh-autosuggestions
  jeffreytse/zsh-vi-mode
)

_zsh_plugin_load() {
  local name=${1:t} dir=$ZSH_PLUGIN_DIR/${1:t} f
  if [[ ! -d $dir ]]; then
    print "zsh: installing plugin $1..."
    git clone -q --depth 1 https://github.com/$1.git $dir || return
  fi
  for f in $dir/$name.plugin.zsh $dir/$name.zsh; do
    [[ -r $f ]] && { source $f; return }
  done
  print -u2 "zsh plugin $1: no entry file found in $dir"
}

zsh-plugins-update() {
  local repo
  for repo in $zsh_plugins; do
    print -n "$repo: "
    git -C $ZSH_PLUGIN_DIR/${repo:t} pull -q --ff-only && git -C $ZSH_PLUGIN_DIR/${repo:t} log -1 --format='%h %s'
  done
}

for _p in $zsh_plugins; do _zsh_plugin_load $_p; done
unset _p
