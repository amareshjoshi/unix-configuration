#!/usr/bin/env bash

#
# copy config files into home directory, deleting
# any old files and folders first

#
# bash
rm -rvf ${HOME}/.bash.d
rm -vf ${HOME}/.bash_logout
rm -vf ${HOME}/.bash_profile
rm -vf ${HOME}/.bashrc
#
# tmux
rm -vf ${HOME}/.tmux.conf
#
# git
rm -vf ${HOME}/.gitconfig
#
# zsh
rm -vf ${HOME}/.zshrc
rm -rvf ${HOME}/.zsh.d
#
#i3
rm -rvf ~/.config/i3
rm -rvf ~/.config/i3status

#
# emacs
EH="${HOME}/.emacs.d"
rm -rvf ${EH}/lisp
rm -rvf ${EH}/lisp-old
rm -rvf ${EH}/themes

#
# now copy files to home
# bash,emacs,tmux,zsh
cp -rv .[betz]* ${HOME}/
cp -v .gitconfig ${HOME}/
cp -rv .config/i3* ${HOME}/.config/




