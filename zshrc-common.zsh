# zshrc-common
#
# Common settings for both interactive and non-interactive zsh. You should
# source this file from ".zshenv". See zshrc-common-interactive for settings
# appropriate to interactive use, like aliases, completion, etc.

export EDITOR=emacsclient

typeset -U path              # Make sure $path doesn't contain duplicates

path+=(/opt/homebrew/bin)
path+=(~/Stuff/bin)
path+=(/Applications/Emacs.app/Contents/MacOS/bin)

# XDG base directory
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"

export ATOM_HOME="${XDG_DATA_HOME}/atom"
export DVDCSS_CACHE="${XDG_DATA_HOME}/dvdcss"
export LESSKEY="${XDG_CONFIG_HOME}/less/lesskey"
export LESSHISTFILE="${XDG_CACHE_HOME}/less/history"
export XAUTHORITY="${XDG_RUNTIME_DIR}/Xauthority"
