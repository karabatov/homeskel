# ~/.profile: executed by Bourne-compatible login shells.

if [ "$BASH" ]; then
  if [ -f ~/.bashrc ]; then
    . ~/.bashrc
  fi
fi

export PS1="\[\e[36;1m\]\u \[\e[32;1m\]\W\[\e[0m\] \$ "
export LANG=en_US.UTF-8
set meta-flag on
set input-meta on
set output-meta on
set convert-meta off
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad
export PATH=/usr/local/opt/ruby/bin:/usr/local/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/libexec:/usr/local/sbin:$PATH

# Setup Amazon EC2 Command-Line Tools
# export EC2_HOME=~/.ec2
# export EC2_KEY=ykar-devz # name only
# export PATH=$PATH:$EC2_HOME/bin
# export EC2_PRIVATE_KEY=`ls $EC2_HOME/pk-*.pem`
# export EC2_CERT=`ls $EC2_HOME/cert-*.pem`

# Set Java home based on whether it's Mac or Linux
ARCHI=`uname -s`
case "$ARCHI" in
    "Darwin")
        export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Home/
    ;;
    "Linux")
        export JAVA_HOME=/usr/lib/jvm/java-6-openjdk/
    ;;
    *)
    ;;
esac

# sjl's t
#alias t='python ~/Dropbox/Documents/Programming/t/t.py --task-dir . --list todo'
#alias tf='t -f'

# ls
# alias ls='ls --color=auto'
alias ll='ls -la'

# apache & mysql
#alias dev-start='mysql.server start && echo "Starting apache..." && sudo apachectl start'
#alias dev-stop='mysql.server stop && echo "Stopping apache..." && sudo apachectl stop'

# mysql
export PATH="$PATH:/usr/local/mysql/bin"

# axel
alias axel="axel -a"

# git
alias Gresettype="git status | grep typechange | awk '{print \$2}' | xargs git checkout"
alias Gmergepbx="git mergetool --tool=mergepbx"
export GITHUB_USERNAME="karabatov"

# Swift
#export PATH=/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin:"$PATH"
#export SDKROOT=`xcrun --show-sdk-path --sdk macosx`

# Bash completion
[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# editor
export VISUAL="nvim"
export EDITOR="nvim"

# proxy
# alias proxy-on="ssh -N root@socks.yurikarabatov.com -D 3128"

# open files
ulimit -n 4096

# Display sleep
alias displaysleep="pmset displaysleepnow"

# Project
alias switch_branch="cd .. && cd babylon-ios && bundle install && bundle exec pod install && git status"

# Nim
export PATH=~/.nimble/bin:$PATH

# Go
export PATH=~/go/bin:$PATH
