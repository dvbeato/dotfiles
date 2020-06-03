#!/usr/bin/env bash


while read p; do
  code --install-extension $p
done < $PWD/extensions.conf

if [[ "$OSTYPE" == "linux-gnu" ]]; then
  ln -s $HOME/.dotfiles/vscode/settings.json $HOME/.config/Code/User/settings.json
  ln -s $HOME/.dotfiles/vscode/keybindings.json $HOME/.config/Code/User/keybindings.json
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
ln -s $HOME/.dotfiles/vscode/settings.json $HOME/Library/Application\ Support/Code/User/settings.json
ln -s $HOME/.dotfiles/vscode/keybindings.json $HOME/Library/Application\ Support/Code/User/keybindings.json
fi
