#!/usr/bin/env bash

# copy files to home
cp -rv .zsh.d ${HOME}/
cp -rv .zshrc ${HOME}/

cp -v .tmux.conf ${HOME}/
cp -v .gitconfig ${HOME}/

cp -rv .config/emacs ${HOME}/.config/
cp -rv .config/nano ${HOME}/.config/
cp -rv .config/i3 ${HOME}/.config/
cp -rv .config/i3status ${HOME}/.config/
cp -rv .config/sway ${HOME}/.config/
cp -rv .config/foot ${HOME}/.config/




