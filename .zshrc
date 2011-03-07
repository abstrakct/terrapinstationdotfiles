# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=50000
SAVEHIST=50000
setopt appendhistory autocd
unsetopt beep
bindkey -e
#bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/rolf/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

autoload -U promptinit
promptinit
prompt adam1


# options
setopt extendedglob

# variables
export EDITOR="vim"

# completion for PID!
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*:*:killall:*' menu yes select
zstyle ':completion:*:killall:*' force-list always

zstyle ':completion:*:pacman:*' force-list always
zstyle ':completion:*:*:pacman:*' menu yes select

zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'


# keybindings
bindkey "\e[1~" beginning-of-line # Home
bindkey "\e[4~" end-of-line # End
bindkey "\e[5~" beginning-of-history # PageUp
bindkey "\e[6~" end-of-history # PageDown
bindkey "\e[2~" quoted-insert # Ins
bindkey "\e[3~" delete-char # Del
bindkey "\e[7~" beginning-of-line # Home
bindkey "\e[8~" end-of-line # End
bindkey "^[[A" history-search-backward
bindkey "^[[B" history-search-forward


# aliases
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
#alias awe="cp .xinitrc.nano .xinitrc && startx"
#alias c3="cal -3m"
alias cal="cal -m"
#alias chat=weechat-curses
alias beersmith="wine /home/rolf/wine/beersmith/BeerSmith.exe"
alias df="df -h"
alias diff=colordiff
alias el=elinks
alias futurama="echo && curl -Is slashdot.org | sed -ne '/^X-[FBL]/s/^X-//p' && echo"
#alias goleia="sudo mount -v -t nfs4 10.0.0.23:/ /mnt/leia"
alias grep="grep --color=always"
alias ls='ls --color=auto -hF --group-directories-first --time-style=+"%Y-%m-%d %H:%M"' 
alias l='ls -l'
alias la='ls -la'
alias m="cd /home/rolf/TheVault/musikk"
alias mv="mv -v"
alias n="sudo /etc/rc.d/network"
alias nano="nano -w"
alias nd="sudo ndiswrapper"
alias netre="sudo /etc/rc.d/network restart"

alias pac='sudo pacman-color'
alias pico=nano
alias psg="ps aux | grep"
#alias rbcl="cat ~/bin/cmus/rebuild-library | cmus-remote"
#alias reload="source ~/.bashrc"
alias t=todo.sh
alias tx="transmission-remote --auth rolf:torrenT -l"
alias x=startx
#alias ya=yaourt
alias xxx="cd /home/rolf/TheVault/.data"

PATH=$PATH:/home/rolf/bin
export VMAIL_HTML_PART_READER='elinks -dump'

SSH_ENV="$HOME/.ssh/environment"

# start the ssh-agent
function start_agent {
    echo "Initializing new SSH agent..."
    # spawn ssh-agent
    ssh-agent | sed 's/^echo/#echo/' > "$SSH_ENV"
    echo succeeded
    chmod 600 "$SSH_ENV"
    . "$SSH_ENV" > /dev/null
    ssh-add
}
#
## test for identities
function test_identities {
    # test whether standard identities have been added to the agent already
    ssh-add -l | grep "The agent has no identities" > /dev/null
    if [ $? -eq 0 ]; then
        ssh-add
        # $SSH_AUTH_SOCK broken so we start a new proper agent
        if [ $? -eq 2 ];then
            start_agent
        fi
    fi
}

## check for running ssh-agent with proper $SSH_AGENT_PID
if [ -n "$SSH_AGENT_PID" ]; then
    ps -ef | grep "$SSH_AGENT_PID" | grep ssh-agent > /dev/null
    if [ $? -eq 0 ]; then
	test_identities
    fi
## if $SSH_AGENT_PID is not properly set, we might be able to load one from
## $SSH_ENV
else
    if [ -f "$SSH_ENV" ]; then
	. "$SSH_ENV" > /dev/null
    fi
    ps -ef | grep "$SSH_AGENT_PID" | grep ssh-agent > /dev/null
    if [ $? -eq 0 ]; then
        test_identities
    else
        start_agent
    fi
fi


echo
fortune
echo
