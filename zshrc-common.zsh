# zshrc-common
#
# Common settings for both interactive and non-interactive zsh. You should
# source this file from ".zshenv". See zshrc-common-interactive for settings
# appropriate to interactive use, like aliases, completion, etc.

export EDITOR=emacsclient

typeset -U path              # Make sure $path doesn't contain duplicates

add_to_path_if_exists() {
  d=$1
  if [[ -d "$d" ]]; then
    path+=("$d")
  fi
}

add_to_path_if_exists "/opt/homebrew/bin"           # Homebrew M1 binaries
add_to_path_if_exists "/usr/local/homebrew/bin"     # Homebrew x86_64 binaries
add_to_path_if_exists "/opt/local/bin"              # MacPorts binaries
add_to_path_if_exists "~/Stuff/bin"
add_to_path_if_exists "/Applications/Emacs.app/Contents/MacOS/bin"
add_to_path_if_exists "/Applications/MacPorts/Emacs.app/Contents/MacOS/bin"

# XDG base directory
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"

export ATOM_HOME="${XDG_DATA_HOME}/atom"
export DVDCSS_CACHE="${XDG_DATA_HOME}/dvdcss"
export LESSKEY="${XDG_CONFIG_HOME}/less/lesskey"
export LESSHISTFILE="${XDG_CACHE_HOME}/less/history"
export XAUTHORITY="${XDG_RUNTIME_DIR}/Xauthority"
