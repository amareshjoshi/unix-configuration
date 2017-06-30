# ~/.bash_logout: executed by bash(1) when login shell exits.

#
# delete command history
# doesn't work, for some reason
#   cat /dev/null > ~/.bash_history
#
# this works
echo "" > ~/.bash_history

# when leaving the console clear the screen to increase privacy

if [ "$SHLVL" = 1 ]; then
    [ -x /usr/bin/clear_console ] && /usr/bin/clear_console -q
fi
