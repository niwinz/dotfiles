#!/usr/bin/env zsh
source ~/powerlevel10k/powerlevel10k.zsh-theme

export LANG="en_US.UTF-8"
export LC_COLLATE="C"
export EDITOR=vim
export PGCLIENTENCODING="utf8"
export PATH="$HOME/bin:/home/niwi/.local/bin:$PATH"

export _JAVA_AWT_WM_NONREPARENTING=1
export JAVA_HOME="/usr/lib/jvm/default"

bindkey -e
bindkey    "^[[3~"          delete-char
bindkey    "^[3;5~"         delete-char
bindkey    '^R'             history-incremental-search-backward

#autoload -U promptinit
#promptinit
#prompt zefram

#------------------------------
## Comp stuff
##------------------------------
zmodload zsh/complist
autoload -Uz compinit
compinit

zstyle :compinstall filename '${HOME}/.zshrc'
zstyle ':completion:*:pacman:*' force-list always
zstyle ':completion:*:*:pacman:*' menu yes select


#------------------------------
# Alias stuff
#------------------------------
alias ls="ls --color -F"
alias ll="ls --color -lh"
alias 'lsd'='ls -d *(/)'
alias 'lsf'='ls -h *(.)'
alias 'rm'='rm -r'
alias 'cp'='cp -r'
alias 'l'='ls --color -GFlh'

#-----------------
# Options
#-----------------

setopt AUTO_CD               # implicate cd for non-commands
#setopt CD_ABLE_VARS       # read vars in cd
setopt CORRECT_ALL            # correct spelling
setopt COMPLETE_IN_WORD    # complete commands anywhere in the word
setopt NOTIFY              # Notify when jobs finish
#setopt C_BASES             # 0xFF
setopt BASH_AUTO_LIST      # Autolist options on repeition of ambiguous args
#setopt CHASE_LINKS         # Follow links in cds
#setopt AUTO_PUSHD          # Push dirs into history
#setopt ALWAYS_TO_END       # Move to the end on complete completion
#setopt LIST_ROWS_FIRST     # Row orientation for menu
setopt MULTIOS             # Allow Multiple pipes
#setopt MAGIC_EQUAL_SUBST   # Expand inside equals
setopt EXTENDED_GLOB
setopt NOBEEP
setopt INC_APPEND_HISTORY

export HISTSIZE=5000
export SAVEHIST=5000
export HISTFILE=~/.zhistory

## OTHER OPTS
setopt hist_ignore_all_dups
setopt hist_ignore_space

#------------------------------
# Window title
#------------------------------
# case $TERM in
#     *xterm*|rxvt|rxvt-unicode|rxvt-256color|(dt|k|E)term)
# 		precmd () { print -Pn "\e]0;$TERM - (%L) [%n@%M]%# [%~]\a" }
# 		preexec () { print -Pn "\e]0;$TERM - (%L) [%n@%M]%# [%~] ($1)\a" }
# 	;;
#     screen)
#     	precmd () {
# 			print -Pn "\e]83;title \"$1\"\a"
# 			print -Pn "\e]0;$TERM - (%L) [%n@%M]%# [%~]\a"
# 		}
# 		preexec () {
# 			print -Pn "\e]83;title \"$1\"\a"
# 			print -Pn "\e]0;$TERM - (%L) [%n@%M]%# [%~] ($1)\a"
# 		}
# 	;;
# esac

bindkey -e

# LEIN_FAST_TRAMPOLINE=y
# export LEIN_FAST_TRAMPOLINE
# alias cljsbuild="lein trampoline cljsbuild $@"
# alias docker-clean="docker ps -a | grep 'weeks ago' | awk '{print $1}' | xargs --no-run-if-empty docker rm"

source ~/.nvm/nvm.sh

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
