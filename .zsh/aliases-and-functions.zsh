#--------------------------------------
# aliases and functions
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

#
# OpenVPN aliases
export OPENVPN_CONFIG=${HOME}/.ssh/openvpn.hcommons.org-client.ovpn
alias openvpn-start="openvpn3 session-start --config ${OPENVPN_CONFIG}"
alias openvpn-list="openvpn3 sessions-list"
alias openvpn-restart="openvpn3 session-manage --config ${OPENVPN_CONFIG} --restart"
alias openvpn-stop="openvpn3 session-manage --config ${OPENVPN_CONFIG} --disconnect"


#--------------------------------------
# functions
# note: use "command ls" to avoid infinite recursion when command name and alias name are the same 
#--------------------------------------
#
# combine less and ls
function l {
    if [[ -d ${1} ]];  then
        ls --human-readable --classify --color=auto ${1}
    else
        command less ${1}
    fi
}
function ls { command ls --human-readable --classify --color=auto ${@} ; }
function la { ls --all --human-readable --classify --color=auto ${@} ; }
function ll { ls -l --human-readable --classify --color=auto ${@} ; }
function lla { ls -l --all --human-readable --classify --color=auto ${@} ; }
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
