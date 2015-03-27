#!/bin/bash
###########################################################
# This script creates symlinks for every $file in $dir to ~
###########################################################

## VARIABLES
dir=~/dotfiles
files="bashrc gitignore_global vimrc bash_profile gitconfig profile tmux.conf"

## GOT TO DIR
echo "Changing to the $dir directory"
cd $dir
echo "...done"

## DELETING .FILES AND CREATING SYMLINKS
for file in $files; do
    echo "Deleting .$file"
    rm ~/.$file
    echo "Creating symlink for .$file"
    ln -s $dir/$file ~/.$file
done
