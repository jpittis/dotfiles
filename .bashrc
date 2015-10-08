# 2 line prompt with git branch
parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

#PS1="\u@\h\[\033[32m\]\$(parse_git_branch)\[\033[00m\] \w\n\$ "
PS1="\$ "

# ls with color and formatting
alias ls="ls -FG"
alias sl="ls"

# use rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# go environment
export GOPATH=$HOME/code/go
export PATH=$PATH:$GOPATH/bin
alias cdg="cd $GOPATH/src/github.com/jpittis"

# archive directory
export ARCHIVE=$HOME/archive/archive
