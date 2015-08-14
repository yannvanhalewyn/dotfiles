#!/bin/sh
###########################################################
# This script creates symlinks for my dotfiles to ~
###########################################################

# Variables
dir=/Users/yannvanhalewyn/dotfiles
files="bash_profile bash_prompt gitignore_global
"

# Got to dir
echo "Changing to the $dir directory"
cd $dir

# Get files
files=$(ls -A | grep -v 'DS_Store\|.git\|Scripts')
echo "Will create symlinks for:"
echo "$files"

# Prompt user
echo "Continue? (Y/n) \c"; read -n1 ans;
if [ $ans != 'y' ]
then
  echo "\nOk! Some other time then."
  exit
fi

# Deleting exising .links and creating symlinks
for file in $files; do
    echo "Deleting ~/.$file"
    rm $HOME/.$file
    echo "Linking $dir/$file -> ~/.$file"
    ln -s $dir/$file ~/.$file
done

# Symlink vim files for neovim
ln -s $dir/vim ~/.nvim
ln -s $dir/vimrc ~/.nvimrc
