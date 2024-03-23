# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

setopt autocd beep extendedglob nomatch notify
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/yecinem/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Plugins
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/omz/stack.plugin.zsh
source ~/.zsh/zsh-vi-mode/zsh-vi-mode.plugin.zsh
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

alias django-test="./manage.py test"
alias django-runserver="./manage.py runserver"
alias django-shell="./manage.py shell"
alias django-migrate="./manage.py migrate"
alias django-makemigrations="./manage.py makemigrations"

## Keybindings
bindkey "^[[3~" delete-char
bindkey "^[[H" beginning-of-line
bindkey "^[[F" end-of-line

## Editor
export EDITOR="emacs -nw"
alias emacs="emacs -nw"

## GHCUP
[ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"
export GHCUP_INSTALL_BASE_PREFIX=$HOME
export PATH=$GHCUP_INSTALL_BASE_PREFIX/.ghcup/bin:$PATH

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

eval "$(direnv hook zsh)"
eval "$(starship init zsh)"
eval "$(thefuck --alias)"
eval "$(zoxide init zsh)"
alias cd=z
