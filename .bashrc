# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

#
# layout:
# standard bash stuff
# my stuff
#    common
#    os (mac, linux) specific
#

# If not running interactively, don't do anything
# $_ lists the options passed to the shell
[[ ! "$-" =~ "i" ]] && return 

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
HISTCONTROL=$HISTCONTROL${HISTCONTROL+:}ignoredups
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
    xterm) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

#
# colors for prompt!
# 32
green="\[\033[0;32m\]"
# 34
blue="\[\033[0;34m\]"
# 35
purple="\[\033[0;35m\]"
reset="\[\033[0m\]"

#
# full hostname
if [ -f "/etc/hostname" ]; then
    fqdn_hostname=$(cat /etc/hostname)
else
    fqdn_hostname=$(hostname)
fi

#
# sets number of directories in the current path to display
# in the prompt. set to 0 to only display the current folder
PROMPT_DIRTRIM=2
if [ "$color_prompt" = yes ]; then
    PS1='${green}\u@${fqdn_hostname}${reset}:${blue}\w${reset} \!\$ '
else
    PS1='\u@${fqdn_hostname}:\w \!\$ '
fi
#unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@${fqdn_hostname}: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    #alias ls='ls --color=auto'
    #alias grep='grep --color=auto'
    #alias fgrep='fgrep --color=auto'
    #alias egrep='egrep --color=auto'
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi


########################################################################
# my stuff
#
#  os specific stuff
#  ... general stuff
#  os specific stuff
########################################################################
#
# bash settings
set -o allexport
set -o emacs

#----------
# Note: Windows WSL only, Linux and Mac don't need to do this
#       because the default path is good.
#       WSL needs this to add some of the windows paths to the linux path
# we want to preserve any previous value for the PATH for Windows WSL
# export PATH=/bin:/usr/bin:/sbin:/usr/sbin:${PATH}
#----------

case `uname -s` in
    Darwin*|Wallace*)
        #
        #PATH=/opt/local/bin:/opt/local/sbin:/usr/local/sbin:${PATH}
        PATH=/usr/local/sbin:${PATH}
        #
        # if GNU coreutils exists put it before BSD utils that come with MacOS
        if [ -d /opt/local/libexec/gnubin ]; then
            PATH=/opt/local/libexec/gnubin:${PATH}
        fi
        export MANPATH=/opt/local/share/man:${MANPATH}
        #export JAVA_HOME=`/usr/libexec/java_home -v 1.x`
        export JAVA_HOME=`/usr/libexec/java_home`
        #
        # MSU computer lab macs only have text emacs
        export EMACS='/usr/bin/emacs'
        #
        # but if MacPorts emacs exists use that 
        if [ -f /Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs ]; then
            export EMACS='/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs'
            export EMACS_BIN="/Applications/MacPorts/Emacs.app/Contents/MacOS/bin"
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
        # cancels out this function in /etc/bashrc
        function update_terminal_cwd { cat /dev/null ; }
        #
        # bash completion from MacPorts
        # this is giving errors. comment out for now (2017.08.25)
        #if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
        #    . /opt/local/etc/profile.d/bash_completion.sh
        #fi
        #--
        #
        # MacPorts version of bash
        if [ -f /opt/local/bin/bash ]; then
            export SHELL="/opt/local/bin/bash"
        fi
        #
        # for emacs term and eshell
        export ESHELL="/opt/local/bin/bash"
        #
        # and the last shall be first
        PATH="${HOME}/bin":${PATH}
        ;;
    Linux*|Solaris*)
        #
        # the default PATH is good. no need to edit it.
        # PATH="${HOME}/bin":${PATH}
        # /usr/local/java -> whatever version of java you want 1.6,7,8
        export JAVA_HOME=/usr/local/java
        export EMACS=emacs
        #export SCALA_HOME=/usr/local/scala
        #export PATH=${PATH}:${SCALA_HOME}/bin
        #export MANPATH=${MANPATH}:${SCALA_HOME}/man
        #
        # for emacs term and eshell
        export ESHELL="/bin/bash"
        ;;
    *)
        PATH="${HOME}/bin":/usr/local/bin:/usr/local/sbin:${PATH}
        #
        # for emacs term and eshell
        export ESHELL="/bin/bash"
        ;;
esac

#
# to make things right for utf-8 stuff (was LANG=C)
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

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
# ocaml settings
#----------------------------------------------------------
# CAML_LD_LIBRARY_PATH=${HOME}/.opam/system/lib/stublibs:/opt/local/lib/ocaml/stublibs; export CAML_LD_LIBRARY_PATH;
# OPAMUTF8MSGS="1"; export OPAMUTF8MSGS;
# OCAML_MANPATH=${HOME}/.opam/system/man
# MANPATH=${OCAML_MANPATH}:${MANPATH};
# PERL5LIB=${HOME}/.opam/system/lib/perl5; export PERL5LIB;
# OCAML_TOPLEVEL_PATH=${HOME}/.opam/system/lib/toplevel; export OCAML_TOPLEVEL_PATH;
# OCAML_PATH=${HOME}/.opam/system/bin
# PATH=${OCAML_PATH}:${PATH}


#----------------------------------------------------------
# matlab settings
#----------------------------------------------------------
# MATLAB=/usr/local/matlab
# MATLAB_HOME=/usr/local/matlab
# MATLABPATH=$MATLABPATH:/usr/local/eeglab:.
# export MATLAB MATLAB_HOME MATLABPATH 
# PATH=$MATLAB/bin:${PATH}

#----------------------------------------------------------
# java settings
#----------------------------------------------------------
#
JDK_HOME=${JAVA_HOME}
export JDK_HOME
PATH=${JAVA_HOME}/bin:${PATH}
#ANT_HOME=/usr/share/ant
#export ANT_HOME
#PATH=${ANT_HOME}/bin:${PATH}

#------------------------------------------------------------------------
# OS specific stuff
# mac and linux
#
# Mac OS X (Darwin)
if [ `uname -s` = "Darwin" ]; then
    echo "Mac OS stuff here"
    #
    # MacTeX
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
    #----------------------------------------------------------
    # lisp/scheme stuff
    #----------------------------------------------------------
    #export RACKET_HOME=/opt/local/racket
    if [ -d RACKET_HOME=/opt/local/racket ]; then
        PATH=${RACKET_HOME}/bin:${PATH}
    fi
fi

# Linux
if [ `uname -s` = "Linux" ]; then
    echo "Linux stuff here"
    #----------------------------------------------------------
    # lisp/scheme stuff
    #----------------------------------------------------------
    #
    # used for locally installed racket
    if [ -d RACKET_HOME=/usr/local/racket ]; then
        PATH=${RACKET_HOME}/bin:${PATH}
    fi
    #
    # not sure why this was there. commented out 2018.02.12
    # IntelliJ
    # PATH=/usr/local/IntelliJIdea15/bin:${PATH}
fi

# Alias definitions.
# do this at the end because it may depend on ENVs above
if [ -f ~/.bash.d/bash_aliases ]; then
    . ~/.bash.d/bash_aliases
fi

#################################################################
# GitHub git stuff
#################################################################
#
# Enable tab completion
source ~/.bash.d/git-completion.sh

#
# Change command prompt
# '\$(__git_ps1)' adds git-related stuff
source ~/.bash.d/git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1

if [ "$color_prompt" = yes ]; then
    PS1="${green}\u@${fqdn_hostname}:${purple}\$(__git_ps1)${reset} \w \!$ "
else
    PS1="\u@${fqdn_hostname}:\$(__git_ps1) \w \!$ "
fi

#
# git config commands (only needs to be run once)
#git config --global core.editor "${EMACS} -n -w"
#git config --global push.default upstream
#git config --global merge.conflictstyle diff3

##
## run tmux (if available)
#if [ -f "/usr/bin/tmux" ]; then
#    echo "Connecting to tmux ..."
#    tm
#fi

#
# --- eof ---
