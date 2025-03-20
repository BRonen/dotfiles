#!/usr/bin/bash

OS_NAME=$(cat /etc/os-release | awk -F= '{ if ($1 == "NAME") print $2}' -)

if [ $OS_NAME = "NixOS" ]; then
    echo "NixOS detected"
else
    echo "$OS_NAME detected"
fi

# remove default config files and replace with custom dotfiles
rm -rf ~/.config/i3 ~/.config/i3status ~/.emacs.d ~/.spacemacs ~/.config/alacritty

mkdir ~/.config/i3 ~/.config/i3status ~/.config/alacritty

# setup of dotfiles using links
ln ./.config/i3/config ~/.config/i3/config
ln ./.config/i3status/config ~/.config/i3status/config
ln ./.config/alacritty/alacritty.toml ~/.config/alacritty/alacritty.toml
ln ./.spacemacs ~/.spacemacs
ln -s $PWD/.emacs.d ~/.emacs.d
