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

### Added by the Heroku Toolbelt
#export PATH="/usr/local/heroku/bin:$PATH"

# mysql
export PATH="$PATH:/usr/local/mysql/bin"

# axel
alias axel="axel -a"

# git
alias git="hub"
alias Gresettype="git status | grep typechange | awk '{print \$2}' | xargs git checkout"

# Haskell
#export PATH="$HOME/Library/Haskell/bin:$PATH"

# Node
export PATH="$HOME/local/bin:$PATH"

# Swift
#export PATH=/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin:"$PATH"
#export SDKROOT=`xcrun --show-sdk-path --sdk macosx`

# completion
if [ "$(uname)" == "Darwin" ]; then
    source /usr/local/etc/bash_completion.d/password-store
    source /usr/local/etc/bash_completion.d/git-completion.bash
    source /usr/local/etc/bash_completion.d/git-flow-completion.bash
fi

# editor
export VISUAL="nvim"
export EDITOR="nvim"

# proxy
# alias proxy-on="ssh -N root@socks.yurikarabatov.com -D 3128"

# open files
ulimit -n 4096

# Racket
# export PATH="/Applications/Racket v6.6/bin:$PATH"

# Todo.txt
alias t="topydo"
