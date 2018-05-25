# ~/.bash_logout: executed by bash(1) when login shell exits.

#
# delete command history
# doesn't work, for some reason
#   cat /dev/null > ~/.bash_history
#
# this works
echo "" > ~/.bash_history

#
# delete any ssh keys when logging out of Ubuntu VMs
case `uname -v` in
    Ubuntu*)
        ssh-add -D
        ;;
    *)
        #
        # for Mac OS, WSL and regular (non Ubuntu) Linux do nothing
        ;;
esac
    

# when leaving the console clear the screen to increase privacy
if [ "$SHLVL" = 1 ]; then
    [ -x /usr/bin/clear_console ] && /usr/bin/clear_console -q
fi
