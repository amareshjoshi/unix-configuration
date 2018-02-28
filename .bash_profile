#
# need to figure out what's supposed to go in what
#
# .bashrc - interactive shell (non login)
# .bash_profile (login shell (non-interactive shells will inherit this))

#
# for now just source .bashrc
#
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi


case `uname -s` in
    Darwin*|Wallace*)
	;;
    Linux*|Solaris*)
	;;
    CYGWIN*)
	#
	# ssh stuff
	SSHAGENT=/usr/bin/ssh-agent
	SSHAGENTARGS="-s"
	if [ -z "$SSH_AUTH_SOCK" -a -x "$SSHAGENT" ]; then
	    eval `$SSHAGENT $SSHAGENTARGS`
	    trap "kill $SSH_AGENT_PID" 0
	fi
	;;
    *)
        ;;
esac
