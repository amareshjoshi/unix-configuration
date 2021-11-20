###########################################################
#
# .zshrc file 
#
#
# basic layout
#
# - stuff added by zsh setup programs (may be moved later)
# - general stuff aliases, functions, prompts, etc
# - first OS specific section
# - more general stuff (depends on os specific settings above)
# - 2nd OS specific section
# - final general stuff
#
###########################################################


#--------------------------------------
# The following lines were added by compinstall
#--------------------------------------
zstyle :compinstall filename '/Users/joshia/.zshrc'

autoload -Uz compinit
compinit
#--------------------------------------
# End of lines added by compinstall
#--------------------------------------

#--------------------------------------
# aliases
#--------------------------------------
# alias h="fc -l"
alias h=history
alias e="emacs -nw"
#
# displays path one dir per line
alias path='echo -e ${PATH//:/\\n}'
alias rot13="tr '[A-Za-z]' '[N-ZA-Mn-za-m]'"
alias dc3="/usr/bin/dc -e 3k - "

#--------------------------------------
# functions
#--------------------------------------
#
# combine less and ls
function l {
    if [[ -d ${1} ]];  then
        /bin/ls --human-readable --classify --color=auto ${1}
    else
        /bin/less ${1}
    fi
}
function ls { /bin/ls --human-readable --classify --color=auto ${@} ; }
function la { /bin/ls --all --human-readable --classify --color=auto ${@} ; }
function ll { /bin/ls -l --human-readable --classify --color=auto ${@} ; }
function lla { /bin/ls -l --all --human-readable --classify --color=auto ${@} ; }
function isit { ps -ef | grep ${@} | grep -v grep ; }
#
# attach to a running tmux, or run a new instance
function tm {
    if [[ $(tmux ls | wc -l) = "0" ]];  then
        tmux
    else
        tmux attach-session
    fi
}
#
# for CCR dev
function testCCR {
    START_DIR=$(pwd)
    echo "PWD = " ${PWD}
    CCR_HOME="${HOME}/development/CCR"
    if [[ -d ${CCR_HOME} ]]; then
        echo "Starting CCR testing ..." && \
            cd ${CCR_HOME}/backend && lando artisan test && \
            cd ${CCR_HOME}/client && lando yarn test:unit && lando cypress run
        cd ${START_DIR}
    fi
}

#--------------------------------------
# other basic shell stuff
#--------------------------------------

#
# command history
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=0
#
# beep on errors
setopt beep
#
# emacs style line editing
bindkey -e
#
# prompt
setopt PROMPT_BANG
setopt PROMPT_SUBST
# %F{color} (%f) = set (stop) using Foreground color
# %K{color} (%k) = set (stop) using bacKground color
# %B (%b) = set (stop) boldface mode
# %n  = username
# %M = full machine name
# %m = just the first part
# %Nd = last n part of pwd
# %Nd = last n part of pwd with tilde substitution
# %! or %h = history number
PROMPT='%B%F{green}%n@%m%f%b: %2~ %h$ '

# to make things right for utf-8 stuff (was LANG=C)
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

#
# first OS specific section
#
# os variables
case $(uname -s) in
    Darwin)
        macos=true
        ;;
    Linux)
        linux=true
        ;;
    *)
        unix=true
        ;;
esac
osname=$(tr '[A-Z]' '[a-z]' <<< $(uname -s))
#
#  ms wsl check
case $(uname -a) in
    *[Mm]icrosoft*)
        wsl=true
        ;;
esac


if [[ "${macos}" ]]; then
    echo "mac os ..."
    #
    PATH=/opt/local/bin:/opt/local/sbin:${PATH}
    #
    # if GNU coreutils exists put it before BSD utils that come with MacOS
    if [[ -d /opt/local/libexec/gnubin ]]; then
        PATH=/opt/local/libexec/gnubin:${PATH}
    fi
    export MANPATH=/opt/local/share/man:${MANPATH}
    #export JAVA_HOME=$(/usr/libexec/java_home -v 1.x)
    export JAVA_HOME=$(/usr/libexec/java_home)
    export EMACS='/Applications/MacPorts/EmacsMac.app/Contents/MacOS/Emacs'
    export EMACS_BIN="/Applications/MacPorts/EmacsMac.app/Contents/MacOS/bin"
    #----------------------------------------------------------
    # lisp/scheme stuff
    #----------------------------------------------------------
    #
    # switched to GNU guile
    #
    #if [[ -d RACKET_HOME=/opt/local/racket ]]; then
    #    PATH=${RACKET_HOME}/bin:${PATH}
    #fi
    # for X11 (linux does it automatically)
    export DISPLAY=":0.0"
    #
    # and the last shall be first
    PATH="${HOME}/bin":${PATH}
fi
#
if [[ "${linux}" ]]; then
    echo "linux ..."
    if [[ "${wsl}" ]]; then
        echo "wsl ..."
        #
        # wsl linux: wsl adds windows path dirs after the the linux path
        # we just need to add in $HOME/bin
        PATH="${HOME}/bin:${PATH}"
    else
        # (non wsl) linux
        # the default PATH (from /etc/profile) doesn't include */sbin
        PATH="${HOME}/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
        PATH="${PATH}:/usr/local/games:/usr/games"
    fi
    #
    # snap
    if [[ -d /snap/bin ]]; then
        PATH=/snap/bin:${PATH}
    fi
    if [[ -d /usr/local/java ]]; then
        # ln -s whatever version of java you want 1.6,7,8 to /usr/local/java
        export JAVA_HOME=/usr/local/java
        export EMACS=emacs
    fi
    #----------------------------------------------------------
    # lisp/scheme stuff
    #----------------------------------------------------------
    #
    # switched to GNU guile
    #
    # used for locally installed racket
    #if [[ -d RACKET_HOME=/usr/local/racket ]]; then
    #    PATH=${RACKET_HOME}/bin:${PATH}
    #fi
    #
    if [[ -f /usr/bin/ssh-askpass ]]; then
        export SSH_ASKPASS=/usr/bin/ssh-askpass
    fi
    # use by some xfce programs (like notes)
    export XDG_DATA_HOME=${HOME}
fi
#
if [[ "${unix}" ]]; then
    echo "other unix os ..."
    PATH="${HOME}/bin":/usr/local/bin:/usr/local/sbin:${PATH}
fi

#
# for all...
export ORGANIZATION="Michigan State University"
export VISUAL=${EMACS}
export EDITOR=${EMACS}
# hitory (fc -l) editor
export FCEDIT=${EMACS}
#
# spelling program
export SPELL=aspell
#
# for emacs term and eshell
export ESHELL=${SHELL}

#----------------------------------------------------------
# TeX settings
#----------------------------------------------------------
# set version
export TEXBINPREFIX=x86_64
if [[ -d /usr/local/texlive/2018 ]]; then
    export TEXYEAR=2018
fi
if [[ -d /usr/local/texlive/2019 ]]; then
    export TEXYEAR=2019
fi
if [[ -d /usr/local/texlive/2020 ]]; then
    export TEXYEAR=2020
fi
if [[ -d /usr/local/texlive/2021 ]]; then
    export TEXYEAR=2021
    if [[ "${osname}" = "darwin" ]]; then
        export TEXBINPREFIX=universal
    fi
fi
# if /usr/local install exists then use it
# otherwise the distribution version will be used
if [[ -d /usr/local/texlive/${TEXYEAR} ]]; then
    export TEXBIN=/usr/local/texlive/${TEXYEAR}/bin/${TEXBINPREFIX}-${osname}
    export TEXINFO=/usr/local/texlive/${TEXYEAR}/texmf-dist/doc/info
    export TEXMAN=/usr/local/texlive/${TEXYEAR}/texmf-dist/doc/man
    PATH=${TEXBIN}:${PATH}
    MANPATH=${TEXMAN}:${MANPATH}
    INFOPATH=${TEXINFO}:${INFOPATH}
fi

#----------------------------------------------------------
# java settings
#----------------------------------------------------------
# only set these if ${JAVA_HOME} is defined
if [[ -d ${JAVA_HOME} ]]; then
    JDK_HOME=${JAVA_HOME}
    export JDK_HOME
    PATH=${JAVA_HOME}/bin:${PATH}
    #ANT_HOME=/usr/share/ant
    #export ANT_HOME
    #PATH=${ANT_HOME}/bin:${PATH}
fi

#----------------------------------------------------------
# GitHub git stuff
#----------------------------------------------------------
#
# enable tab completion
fpath=(~/.zsh $fpath)
#
# change command prompt
# '$(__git_ps1)' adds git-related stuff
source ~/.bash.d/git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
PROMPT='%B%F{green}%n@%m%f%b: %2~%B%F{yellow}$(__git_ps1)%f%b %h$ '

#----------------------------------------------------------
# nvm stuff
#----------------------------------------------------------
export NVM_DIR=${HOME}/.nvm
# sources the fille if (&&) it exists
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm


#------------------------------------------------------------------------
# final os specific stuff
#
# mac os
if [[ "${macos}" ]]; then
    echo "final mac os stuff"
fi
#
# linux
if [[ "${linux}" ]]; then
    echo "final linux stuff"
fi
#
# ms wsl
if [[ "${wsl}" ]]; then
    echo "final wsl stuff"
    source ~/.zsh/config-ssh-agent.zsh
fi


#
# --- eof ---
#
