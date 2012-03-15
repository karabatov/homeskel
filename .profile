# ~/.profile: executed by Bourne-compatible login shells.

if [ "$BASH" ]; then
  if [ -f ~/.bashrc ]; then
    . ~/.bashrc
  fi
fi

export PS1="\[\e[36;1m\]\u \[\e[32;1m\]\W\[\e[0m\] \$ "
export LANG=ru_RU.UTF-8
set meta-flag on
set input-meta on
set output-meta on
set convert-meta off
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad
export PATH=/usr/local/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/libexec:$PATH

# Virtualenv helpers
# FIX TO USE LOCAL VENV
export PATH=$HOME/local/bin:/usr/local/share/python:$PATH
source /usr/local/share/python/virtualenvwrapper.sh

has_virtualenv() {
    if [ -e .venv ]; then
        workon `cat .venv`
    fi
}
venv_cd () {
    cd "$@" && has_virtualenv
}
alias cd='venv_cd'

export VIRTUALENV_USE_DISTRIBUTE=true

# Setup Amazon EC2 Command-Line Tools
export EC2_HOME=~/.ec2
export PATH=$PATH:$EC2_HOME/bin
export EC2_PRIVATE_KEY=`ls $EC2_HOME/pk-*.pem`
export EC2_CERT=`ls $EC2_HOME/cert-*.pem`
export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Home/

# sjl's t
alias t='python ~/Dropbox/Documents/Programming/t/t.py --task-dir . --list todo'
alias tf='t -f'

# Highrise console, see github.com/karabatov/ht
source ~/homeskel/highrise.sh
alias ht='python ~/Dropbox/Documents/Programming/ht/ht.py'
alias htf='ht -f'

# Fix MacVim's buffer issue
alias gvim='open -a MacVim'

# tmux
alias ta='tmux attach'
