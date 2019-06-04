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
# - more general stuff (depenfs on os specific settings above)
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
alias e="emacs -nw"
alias l=more
#
# displays path one dir per line
alias path='echo -e ${PATH//:/\\n}'
alias rot13="tr '[A-Za-z]' '[N-ZA-Mn-za-m]'"
alias dc3="/usr/bin/dc -e 3k - "
#
# graphical version of mac emacs
alias macem='/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs'

#--------------------------------------
# functions
#--------------------------------------
function la { ls -haF ${@} ; }
function ll { ls -hlaF ${@} ; }
function lla { ls -hlaF ${@} ; }
function isit { ps -ef | grep ${@} | grep -v grep ; }
#
# attach to a running tmux, or run a new instance
function tm {
    if [ `tmux ls | wc -l` = "0" ];  then
        tmux
    else
        tmux attach-session
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
#
# need to trim pwd like in bash
PROMPT='%n@%m: `pwd` !$ '
#
# with color
# %F{color} (%f) = set (stop) using Foreground color
# %K{color} (%k) = set (stop) using bacKground color
# %B (%b) = set (stop) boldface mode
# %n  = username
# %M = full machine name
# %m = just the first part
# %Nd = last n part of pwd
# %Nd = last n part of pwd with tilde substitution
# %! or %h = history number
PROMPT='%B%F{green}%n@%M%f%b: %2~ %h$ '

# to make things right for utf-8 stuff (was LANG=C)
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

#
# first OS specific section
#----------
# Note: Windows WSL only, Linux and Mac don't need to do this
#       because the default path is good.
#       WSL needs this to add some of the windows paths to the linux path
# we want to preserve any previous value for the PATH for Windows WSL
# export PATH=/bin:/usr/bin:/sbin:/usr/sbin:${PATH}
#----------

case `uname -s` in
    Darwin*|Wallace*)
        echo "mac os ..."
        #
        PATH=/opt/local/bin:/opt/local/sbin:${PATH}
        #
        # if GNU coreutils exists put it before BSD utils that come with MacOS
        if [ -d /opt/local/libexec/gnubin ]; then
            PATH=/opt/local/libexec/gnubin:${PATH}
        fi
        export MANPATH=/opt/local/share/man:${MANPATH}
        #export JAVA_HOME=`/usr/libexec/java_home -v 1.x`
        export JAVA_HOME=`/usr/libexec/java_home`
        #
        # if MacPorts emacs exists use that
        if [ -f /Applications/Emacs.app/Contents/MacOS/Emacs ]; then
            # !!!! TEMPORARY till macports version of Emacs.app is fixed
            export EMACS='/Applications/Emacs.app/Contents/MacOS/Emacs'
            export EMACS_BIN="/Applications/Emacs.app/Contents/MacOS/bin"
        elif [ -f /Applications/MacPorts/EmacsMac.app/Contents/MacOS/Emacs ]; then
            export EMACS='/Applications/MacPorts/EmacsMac.app/Contents/MacOS/Emacs'
            export EMACS_BIN="/Applications/MacPorts/EmacsMac.app/Contents/MacOS/bin"
        else
            #
            # MSU computer lab macs only have text emacs
            export EMACS='/usr/bin/emacs'
        fi
        #----------------------------------
        # MacTeX
        #----------------------------------
        export TEXYEAR=2018
        # MSU lab machines have TeX 2011 (NEED to better test!)
        if [ -d TEXBIN=/usr/local/texlive/2011 ]; then
            export TEXYEAR=2011
        fi
        export TEXBIN=/usr/local/texlive/${TEXYEAR}/bin/x86_64-darwin
        export TEXINFO=/usr/local/texlive/${TEXYEAR}/texmf-dist/doc/info
        export TEXMAN=/usr/local/texlive/${TEXYEAR}/texmf-dist/doc/man
        PATH=${TEXBIN}:${PATH}
        MANPATH=${TEXMAN}:${MANPATH}
        INFOPATH=${TEXINFO}:${INFOPATH}
        #----------------------------------
        #----------------------------------------------------------
        # lisp/scheme stuff
        #----------------------------------------------------------
        #export RACKET_HOME=/opt/local/racket
        if [ -d RACKET_HOME=/opt/local/racket ]; then
            PATH=${RACKET_HOME}/bin:${PATH}
        fi
        #
        # go lang
        if [ -d GO_HOME="/usr/local/go" ]; then
            PATH=${GO_HOME}/bin:${PATH}
        fi
        #
        # perl6
        if [ -d PERL6_HOME="/Applications/Rakudo" ]; then
            PATH=${PERL6_HOME}/bin:${PATH}
        fi
        # for X11 stuff (linux does it automatically)
        export DISPLAY=":0.0"
        #
        # MacPorts version of zsh????? do we need this?
        if [ -f /opt/local/bin/zsh ]; then
            export SHELL="/opt/local/bin/zsh"
        fi
        #
        # for emacs term and eshell
        export ESHELL="/opt/local/bin/zsh"
        #
        # and the last shall be first
        PATH="${HOME}/bin":${PATH}
        ;;
    Linux*|Solaris*)
        echo "linux ..."
        #
        # the default PATH (from /etc/profile) doesn't include */sbin
        PATH="${HOME}/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
        PATH="${PATH}:/usr/local/games:/usr/games"
        # /usr/local/java -> whatever version of java you want 1.6,7,8
        export JAVA_HOME=/usr/local/java
        export EMACS=emacs
        #----------------------------------
        # MacTeX
        #----------------------------------
        export TEXYEAR=2018
        # if local install exists then use it
        # otherwise the distribution version will be used
        if [ -d /usr/local/texlive/${TEXYEAR} ]; then
            export TEXBIN=/usr/local/texlive/${TEXYEAR}/bin/x86_64-linux
            export TEXINFO=/usr/local/texlive/${TEXYEAR}/texmf-dist/doc/info
            export TEXMAN=/usr/local/texlive/${TEXYEAR}/texmf-dist/doc/man
            PATH=${TEXBIN}:${PATH}
            MANPATH=${TEXMAN}:${MANPATH}
            INFOPATH=${TEXINFO}:${INFOPATH}
        fi
        #----------------------------------
        #----------------------------------------------------------
        # lisp/scheme stuff
        #----------------------------------------------------------
        # used for locally installed racket
        if [ -d RACKET_HOME=/usr/local/racket ]; then
            PATH=${RACKET_HOME}/bin:${PATH}
        fi
        #
        # for emacs term and eshell
        export ESHELL="/bin/zsh"
        ;;
    *)
        echo "other unix os ..."
        PATH="${HOME}/bin":/usr/local/bin:/usr/local/sbin:${PATH}
        #
        # for emacs term and eshell
        export ESHELL="/bin/zsh"
        ;;
esac

#
# for all...
export ORGANIZATION="Michigan State University"
export VISUAL=${EMACS}
export EDITOR=${EMACS}
export FCEDIT=${EMACS}
export SSH_ASKPASS=/usr/bin/ssh-askpass
# use by some xfce programs (like notes)
export XDG_DATA_HOME=${HOME}
#
# spelling program
export SPELL=aspell

#----------------------------------------------------------
# java settings
#----------------------------------------------------------
# only set these if ${JAVA_HOME} is defined
if [ ! ${JAVA_HOME} = "" ]; then
    JDK_HOME=${JAVA_HOME}
    export JDK_HOME
    PATH=${JAVA_HOME}/bin:${PATH}
    #ANT_HOME=/usr/share/ant
    #export ANT_HOME
    #PATH=${ANT_HOME}/bin:${PATH}
fi
#------------------------------------------------------------------------
# final os specific stuff
#
# mac os
if [ `uname -s` = "Darwin" ]; then
    echo "final mac os stuff"
fi

# linux
if [ `uname -s` = "Linux" ]; then
    echo "final linux stuff"
fi

#
# --- eof ---
#
