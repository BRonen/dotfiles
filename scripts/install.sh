#!/usr/bin/bash

OS_NAME=$(cat /etc/os-release | awk -F= '{ if ($1 == "NAME") print $2 }' -)

echo "$OS_NAME detected"

# remove default config files and replace with custom dotfiles
# rm -rf ~/.config/i3 ~/.config/i3status ~/.config/doom ~/.config/emacs ~/.spacemacs ~/.config/alacritty ~/.config/nvim ~/.config/home-manager

# mkdir ~/.config/i3 ~/.config/i3status ~/.config/alacritty ~/.config/nvim

# setup of dotfiles using links
# ln -s $PWD/.config/i3 ~/.config
# ln -s $PWD/.config/i3status ~/.config
# ln -s $PWD/.config/alacritty ~/.config
ln -s $PWD/.config/emacs ~/.config
ln -s $PWD/.config/doom ~/.config
ln -s $PWD/.config/nvim ~/.config
ln -s $PWD/.config/home-manager ~/.config
