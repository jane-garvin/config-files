if status is-interactive
    # Commands to run in interactive sessions can go here
    abbr -a -- q exit
    abbr -a -- cpi 'cp -i'
    abbr -a -- mvi 'mv -i'
    abbr -a -- gb 'git branch'
    abbr -a -- gd 'git diff'
    abbr -a -- gf 'git fetch --all'
    abbr -a -- gs 'git status'
    abbr -a -- gl 'git log --abbrev-commit --graph --all'
end
