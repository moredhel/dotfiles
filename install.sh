#!/bin/bash

DOT_DIR="$HOME/.home"

cd $HOME

# Clone Dotfiles.
git clone https://github.com/hamhut1066/dotfiles.git $DOT_DIR

# Pull Dependencies.

# Spacemacs
git clone --recursive https://github.com/syl20bnr/spacemacs ~/.emacs.d

# oh-my-zsh
git clone --depth=1 --recursive https://github.com/robbyrussell/oh-my-zsh $HOME/.oh-my-zsh

# private layers
git clone https://github.com/hamhut1066/private-layers.git $HOME/.spacemacs.private

# Linking in config files
ln -s $DOT_DIR/.zshrc $HOME/.zshrc
ln -s $DOT_DIR/.spacemacs $HOME/.spacemacs

touch $HOME/.zshrc_local
# Notes

echo "Link from $HOME/.spacemacs.private into $HOME/.emacs.d/private the layers that you want."
echo "put config local to this computer in .zshrc_local"
