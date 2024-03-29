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
zstyle :compinstall filename "${HOME}/.zshrc"

#
# git prompt
autoload -Uz compinit && compinit
#--------------------------------------
# End of lines added by compinstall
#--------------------------------------

#--------------------------------------
source ${HOME}/.zsh.d/aliases-and-functions.zsh

#--------------------------------------
# basic shell stuff
#--------------------------------------
#
# command history
setopt APPEND_HISTORY
# disable Apple medelling (see /etc/zshrc_Apple_Terminal)
SHELL_SESSION_HISTORY=0
HISTFILE=${HOME}/.histfile
HISTSIZE=1000
SAVEHIST=1000
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
# flatpack
export XDG_DATA_DIRS=${XDG_DATA_DIRS}:/home/joshia/flatpak/exports/share

#
# for wayland
# see https://github.com/swaywm/sway/wiki#troubleshooting
export QT_QPA_PLATFORM=wayland
export XDG_SESSION_TYPE=wayland
export QT_WAYLAND_FORCE_DPI=physical
# older versions of Qt
export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
# firefox
export MOZ_ENABLE_WAYLAND=1
# libre office
export SAL_USE_VCLPLUGIN=gtk3
if [ "$XDG_SESSION_DESKTOP" = "sway" ] ; then
    # https://github.com/swaywm/sway/issues/595
    export _JAVA_AWT_WM_NONREPARENTING=1
fi

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
	# special case for Windows TeXLive
	osname=win32
        ;;
esac


if [[ "${macos}" ]]; then
    echo "mac os ..."
    #
    # for some reason /opt/local/[s]bin is getting into the PATH
    # so set it explicitly
    PATH=/usr/local/bin:/usr/local/sbin:/bin:/usr/bin:/sbin:/usr/sbin
    if [[ -d /usr/local/opt/openjdk/bin ]]; then
	export JAVA_HOME=/usr/local/opt/openjdk
	PATH="${JAVA_HOME}/bin:${PATH}"
    fi
    if [[ -d /usr/local/opt/coreutils/libexec/gnubin ]]; then
	COREUTILS=/usr/local/opt/coreutils/libexec/gnubin
	PATH="${COREUTILS}:${PATH}"
    fi
    export EMACSNW="emacsclient -nw --alternate-editor=\"\""
    export EMACS="emacsclient --no-wait --create-frame --alternate-editor=\"\""
    #----------------------------------------------------------
    # lisp/scheme stuff
    #----------------------------------------------------------
    #
    # Racket
    #
    if [[ -d /usr/local/racket ]]; then
	RACKET_HOME=/usr/local/racket
	PATH=${RACKET_HOME}/bin:${PATH}
    fi
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
	export EMACSNW="emacsclient -nw --alternate-editor=\"\""
	export EMACS="emacsclient --no-wait --create-frame --alternate-editor=\"\""
    fi
    #----------------------------------------------------------
    # lisp/scheme stuff
    #----------------------------------------------------------
    #
    # Racket
    #
    # used for locally installed racket
    if [[ -d RACKET_HOME=/usr/local/racket ]]; then
       PATH=${RACKET_HOME}/bin:${PATH}
    fi
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
export EDITOR=${EMACSNW}
# hitory (fc -l) editor
export FCEDIT=${EMACSNW}
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
if [[ "${osname}" = "darwin" ]]; then
    export TEXBINPREFIX=universal
fi
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
fi
if [[ -d /usr/local/texlive/2022 ]]; then
    export TEXYEAR=2022
fi
if [[ -d /usr/local/texlive/2023 ]]; then
    export TEXYEAR=2023
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
fpath=(${HOME}/.zsh.d $fpath)
#
# change command prompt
# '$(__git_ps1)' adds git-related stuff
source ${HOME}/.zsh.d/git-prompt.sh
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
    #
    # Connect to and initalize gnome-keyring-daemon when in sway session
    # to access ssh-agent
    if [ "$DESKTOP_SESSION" = "sway" ]; then
        #
        # this relies on gnome-keyring-daemon to access ssh-agent
        export $(gnome-keyring-daemon --start)
    fi
fi
#
# ms wsl
if [[ "${wsl}" ]]; then
    echo "final wsl stuff"
    source ${HOME}/.zsh.d/config-ssh-agent.zsh
fi

#
# --- eof ---
#
