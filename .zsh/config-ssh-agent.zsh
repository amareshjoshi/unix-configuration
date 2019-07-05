#!/usr/bin/env zsh

#
# ms wsl needs to set ssh-agent configuraion manually
# (do NOT double-quote regex's)
if [[ $(uname -a) =~ .*Microsoft.* ]]; then
    echo "setup ssh-agent ..."
    if [ -z "$(pgrep ssh-agent)" ]; then
        rm -rf /tmp/ssh-*
        eval $(ssh-agent -s) > /dev/null
    else
        export SSH_AGENT_PID=$(pgrep ssh-agent)
        export SSH_AUTH_SOCK=$(find /tmp/ssh-* -name agent\.\*)
    fi
fi

echo "SSH_AGENT_PID = ${SSH_AGENT_PID}"
echo "SSH_AUTH_SOCK = ${SSH_AUTH_SOCK}"



