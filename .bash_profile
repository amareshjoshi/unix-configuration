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

