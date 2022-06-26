#!/usr/bin/env bash

#
# #delete old files and directories
# copy config files into home directory,
#---------------------------------------
# # bash
# rm -rvf ${HOME}/.bash.d
# rm -vf ${HOME}/.bash_logout
# rm -vf ${HOME}/.bash_profile
# rm -vf ${HOME}/.bashrc
# #
# # tmux
# rm -vf ${HOME}/.tmux.conf
# #
# # git
# rm -vf ${HOME}/.gitconfig
# #
# # zsh
# rm -vf ${HOME}/.zshrc
# rm -rvf ${HOME}/.zsh.d
# #
# #.config/various
# rm -rvf ~/.config/i3
# rm -rvf ~/.config/i3status
# rm -rvf ~/.config/sway
# rm -rvf ~/.config/foot

# #
# # emacs
# EH="${HOME}/.emacs.d"
# rm -rvf ${EH}/lisp
# rm -rvf ${EH}/lisp-old
# rm -rvf ${EH}/themes
#---------------------------------------

#
# now copy files to home
# bash,emacs,tmux,zsh
cp -rv .bash* ${HOME}/
cp -rv .emacs* ${HOME}/
cp -rv .zsh* ${HOME}/

cp -v .tmux.conf ${HOME}/
cp -v .gitconfig ${HOME}/

cp -rv .config/i3 ${HOME}/.config/
cp -rv .config/i3status ${HOME}/.config/
cp -rv .config/sway ${HOME}/.config/
cp -rv .config/foot ${HOME}/.config/




