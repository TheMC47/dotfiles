# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=50000
SAVEHIST=50000
# share_history: every shell sees commands from the others as they're run.
# hist_ignore_space: a command starting with a space isn't saved (secrets).
setopt share_history hist_ignore_all_dups hist_reduce_blanks hist_ignore_space

setopt autocd beep extendedglob nomatch notify
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/yecinem/.zshrc'

autoload -Uz compinit
compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' special-dirs true

# End of lines added by compinstall

# Plugins
source ~/.zsh/plugins.zsh
source ~/.zsh/omz/stack.plugin.zsh
ZVM_CURSOR_STYLE_ENABLED=false

colors() {
	local fgc bgc vals seq0

	printf "Color escapes are %s\n" '\e[${value};...;${value}m'
	printf "Values 30..37 are \e[33mforeground colors\e[m\n"
	printf "Values 40..47 are \e[43mbackground colors\e[m\n"
	printf "Value  1 gives a  \e[1mbold-faced look\e[m\n\n"

	# foreground colors
	for fgc in {30..37}; do
		# background colors
		for bgc in {40..47}; do
			fgc=${fgc#37} # white
			bgc=${bgc#40} # black

			vals="${fgc:+$fgc;}${bgc}"
			vals=${vals%%;}

			seq0="${vals:+\e[${vals}m}"
			printf "  %-9s" "${seq0:-(default)}"
			printf " ${seq0}TEXT\e[m"
			printf " \e[${vals:+${vals+$vals;}}1mBOLD\e[m"
		done
		echo; echo
	done
}

alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias np='nano -w PKGBUILD'
alias sspi="ssh pi@192.168.1.2"
alias ls="ls --color=auto"
alias grep="grep --color=auto"

## Keybindings
# zsh-vi-mode sets up its keymaps at the first prompt, wiping bindings made
# earlier in this file, so make them in its after-init hook instead.
zvm_after_init() {
  bindkey "^[[3~" delete-char
  bindkey "^[[H" beginning-of-line
  bindkey "^[[F" end-of-line
}

## Editor
export EDITOR="emacsclient -c -a '' -F '((name . \"float-emacs\"))'"

export BROWSER="/usr/bin/brave"

## GHCUP
[ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"
export GHCUP_INSTALL_BASE_PREFIX=$HOME
export PATH=$GHCUP_INSTALL_BASE_PREFIX/.ghcup/bin:$PATH

## Cabal
export PATH=$HOME/.cabal/bin:$PATH

# BAT
export BAT_THEME="Sublime Snazzy"
alias cat="bat"

# ex - archive extractor
# usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1     ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# Add doom binary to the path
export PATH=$PATH:$HOME/.config/emacs/bin

# User-local binaries (claude, npm globals)
export PATH="$HOME/.local/bin:$PATH"

eval "$(direnv hook zsh)"
eval "$(starship init zsh)"
if [[ "$CLAUDECODE" != "1" ]]; then
    eval "$(zoxide init --cmd cd zsh)"
fi
autoload -U add-zsh-hook

# thefuck takes ~110ms to initialise, so only load it the first time `fuck` runs:
# this stub replaces itself with the real function, then calls it.
fuck() {
  unfunction fuck
  eval "$(thefuck --alias)"
  fuck "$@"
}

# Drop duplicate PATH entries (the arrays `path` and `PATH` are linked).
typeset -U path PATH
