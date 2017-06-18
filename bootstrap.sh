#!/bin/bash

# First check for homeskel
if [ -d "$HOME/homeskel" ]; then
    git clone git://github.com/karabatov/homeskel.git "$HOME/homeskel"
fi

# Clean up
rm -rf "$HOME/.vim"
rm "$HOME/.vimrc"
rm "$HOME/.profile"
rm "$HOME/.tmux.conf"
rm -rf "$HOME/.config/nvim"

# Check for .config
if [ -d "$HOME/.config" ]; then
    mkdir "$HOME/.config"
fi

# Symlinks
# ln -s "$HOME/homeskel/.vim" "$HOME/.vim"
# ln -s "$HOME/homeskel/.vimrc" "$HOME/.vimrc"
ln -s "$HOME/homeskel/.profile" "$HOME/.profile"
ln -s "$HOME/homeskel/.tmux.conf" "$HOME/.tmux.conf"
ln -s "$HOME/homeskel/nvim" "$HOME/.config/nvim"
source "$HOME/.profile"
