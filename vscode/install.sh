#!/bin/bash

while read p; do
  code --install-extension $p
done < $PWD/extensions.conf

ln -s $HOME/.dotfiles/vscode/settings.json $HOME/.config/Code/User/settings.json
ln -s $HOME/.dotfiles/vscode/keybindings.json $HOME/.config/Code/User/keybindings.json