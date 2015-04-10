#!/bin/bash
###########################################################
# This script creates symlinks for every $file in $dir to ~
###########################################################

## VARIABLES
dir=~/dotfiles
files="bashrc gitignore_global vimrc bash_profile gitconfig profile tmux.conf functions aliases osx"

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

## Linking .vim folders
echo "Removing ~/.vim/plugin folder"
rm -ri ~/.vim/plugin
echo "Linking to .vim/plugin"
ln -s $dir/vim/plugin/ ~/.vim/plugin

echo "Linking new profile"
source ~/.bash_profile
