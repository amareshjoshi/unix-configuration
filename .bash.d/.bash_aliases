#!/bin/bash

#
# useful aliases and functions
# sourced in in .bashrc

#
# aliases
#
alias h="fc -l"
alias e=emacs
alias l=more
alias path='echo -e ${PATH//:/\\n}'
alias root="su -"
alias nt="netstat | grep $(hostname) | sort | more"
alias rot13="tr '[A-Za-z]' '[N-ZA-Mn-za-m]'"
alias xterm="xterm -fg white -bg darkslateblue -T xterm\@$(uname -n)"
alias dc3="/usr/bin/dc -e 3k - "
alias macem='/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs'

#
# and functions
#
function ll { ls -hlF ${@} | more ; }
function la { ls -haF ${@} ; }
function lla { ls -lhaF ${@} | more ; }
function isit { ps -ef | grep ${@} | grep -v grep ; }

function p { ${@} | more ; }

#
# attach to a running tmux, or run a new instance
function tm {
    if [ `tmux ls | wc -l` = "0" ];  then
        tmux
    else
        tmux attach-session
    fi
}


#
# sets precision on dc
function dc { 
    /usr/bin/dc -e 3k - 
}

function setserv {
 if [[ ${#} -gt 1 ]];  then
    echo "\nUsage:"
    echo "       setserv {COMSERV|SYBDEV|SYBTEST}\n"
else
    DSLISTEN=${@:-DEVSERV}
    DSQUERY=${@:-DEVSERV}
    export DSLISTEN
    export DSQUERY
 fi
    echo "\nCurrent:"
    echo "        DSLISTEN = $DSLISTEN"
    echo "        DSQUERY = $DSQUERY\n"
}

#
# --- eof ---
#
