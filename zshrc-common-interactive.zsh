# Common settings for interactive zsh only. See zsh-common for setting $EDITOR,
# $PATH, etc.

# required to allow emacs tramp to access remote files
if [[ "$TERM" == "dumb" ]]
then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  unfunction precmd
  unfunction preexec
  PS1='$ '
fi

# use fancy completions
autoload -Uz compinit
compinit

# Set prompt, unless I'm using the spaceship prompt from oh-my-zsh.
if [[ ! -d "${HOME}/.oh-my-zsh" ]]
then
  export PS1="%B%D{%F %r} %~
%# %b"
fi

# Ctrl-r searches history backward. Add Meta-r to search history forward.
bindkey '\er' history-incremental-search-forward

# Unbind Ctrl-left, Ctrl-right, Ctrl-up, and Ctrl-down. I never use Ctrl-left or
# Ctrl-right in the terminal (option-left and option-right work fine to move
# left or right by words), and when I use zsh within emacs, I always want
# windmove to use Ctrl-arrows to move around between windows.
bindkey -r "^[[1;5A"
bindkey -r "^[[1;5B"
bindkey -r "^[[1;5C"
bindkey -r "^[[1;5D"

alias ls='ls -F'
alias q=exit
alias c='cp -i'
alias m='mv -i'
alias mj='make -j8'
alias ds='dirs -pv'
alias less='less --RAW-CONTROL-CHARS'
alias gb='git branch'
alias gd='git diff'
alias gf='git fetch --all'
alias gs='git status'
alias gl='git log --abbrev-commit --graph --all'
alias xyzzy='echo "A hollow voice says \"Fool.\""'
alias t='unset TMPDIR ; tmux has-session 2>/dev/null && tmux attach || tmux'
alias pup='sudo port selfupdate && sudo port upgrade outdated'
s () {
  local t=/opt/local/bin/tmux
  ssh "${1/\"/\\\"}" -t "unset TMPDIR ; $t has-session && exec $t attach || exec $t"
}

# Display a dialog box with the given message
hey () {
  local text="${*/\"/\\\"}"
  printf "\a${text}\n"
  osascript -e "display alert \"${text}\" buttons {\"OK\"}" > /dev/null
}

# Sleep n minutes
msleep () { sleep $(( ${1} * 60 )) }

# Open emacs if it's not open already, and tell it to open files given as
# command line arguments
e () {
  # If there are no arguments, just focus emacs
  if (( ${#} == 0 ))
  then
    echo "tell application \"Emacs.app\" to activate \n return" | osascript
  else
    # If there are arguments, try using emacsclient
    local createframe=""
    if (( ! ${+INSIDE_EMACS} ))
    then
      createframe="--create-frame"
    fi
    emacsclient ${createframe} --no-wait $* 2>/dev/null
  fi ||
  # If emacsclient failed (usually because emacs isn't already open), open it
  # with Applescript, giving it the filename args
  (
    local eargs=()
    for i in $*
    do
      # make each arg an absolute filename and put "" around it
      eargs+=\"${i:a}\"
    done
    # If there's more than one arg, insert commas between them and surround them
    # with {}.
    if (( ${#} > 1 ))
    then
      eargs={${(j:,:)eargs}}
    fi
    echo "tell application \"Emacs.app\" \n activate \n open ${eargs} \n end tell \n return" | osascript
  ) ||
  # Finally, if starting Emacs.app failed, start emacs on the command line
  term=xterm-256color emacs $*
}

# Print the given lines of a file to stdout. Example: 'lines 10-15 foo.txt'
# prints lines 10 through 15.
lines () {
  range=$1
  file=${2:-}  # $file is $2 if it exists
               # otherwise empty so that head and tail will use stdin
  if [[ "$range" =~ "^([0-9]+)-([0-9]+)$" ]]; then
    first=$match[1]
    last=$match[2]
    head -n $last $file | tail -n $(($last - $first + 1))
  elif [[ "$range" =~ "^([0-9]+)-$" ]]; then
    first=$match[1]
    tail -n +$first $file
  elif [[ "$range" =~ "^-([0-9]+)$" ]]; then
    last=$match[1]
    head -n $last $file
  elif [[ "$range" =~ "^([0-9]+)$" ]]; then
    line=$match[1]
    head -n $line $file | tail -n 1
  else
    echo lines: bad range
  fi
}

# Tell emacs to open a directory with dired
d () {
  if (( ${#} == 0 ))
  then
    name=$(pwd)
  elif (( ${#} == 1 ))
  then
    name=$1
  else
    echo d: too many arguments
    return
  fi
  emacsclient --quiet --no-wait --eval "(dired \"${name}\")"
}

# convert numbers between decimal and (64-bit) hexadecimal
h2d() { perl -e "print unpack('q', pack('Q', hex('$1'))), \"\n\"" }
d2h() { perl -e "printf(\"0x%x\n\",$1)" }

# use physical directory structure when following symlinks
setopt chase_links

# Don't use ^D to exit
setopt ignore_eof

# enable real history
export HISTFILE=~/.history
export HISTSIZE=10000
export SAVEHIST=10000

# Append rather than overwrite history on disk
setopt inc_append_history

# tab complete in the middle of a word
setopt complete_in_word

# add things like ^foo (files not named foo)
setopt extended_glob

# don't include slashes in file paths as part of a word
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# case-insensitive tab completion for filenames
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# allow '#' comments interactively
setopt interactivecomments

# I want undefined variables to be an error
# turning off because it breaks zsh spaceship prompt
#setopt nounset

# Define a config file for ripgrep.
export RIPGREP_CONFIG_PATH="${HOME}/.config/rg"
