#!/usr/bin/env bash

#
# copy config files into home directory, deleting
# any old files and folders first

rm -rvf ${HOME}/.bash.d
rm -vf ${HOME}/.bash_logout
rm -vf ${HOME}/.bash_profile
rm -vf ${HOME}/.bashrc
rm -vf ${HOME}/.tmux.conf
rm -vf ${HOME}/.gitconfig



EH="${HOME}/.emacs.d"

rm -rvf ${EH}/lisp
rm -rvf ${EH}/lisp-old
rm -rvf ${EH}/themes

#
# now copy files to home

cp -rv .[bet]* ${HOME}/
cp -v .gitconfig ${HOME}/




