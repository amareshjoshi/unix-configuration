#!/usr/bin/env bash

# copy files to home
# bash,emacs,tmux,zsh
cp -rv .bash* ${HOME}/
cp -rv .zsh* ${HOME}/

cp -v .tmux.conf ${HOME}/
cp -v .gitconfig ${HOME}/

cp -rv .config/emacs ${HOME}/.config/
cp -rv .config/i3 ${HOME}/.config/
cp -rv .config/i3status ${HOME}/.config/
cp -rv .config/sway ${HOME}/.config/
cp -rv .config/foot ${HOME}/.config/




