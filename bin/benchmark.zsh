#!/bin/zsh

export TIMEFMT='%E'

main() {
  echo "Purgin the disk cache.."
  sudo purge

  # ZSH time
  echo "Testing zsh"
  zsh_elapsed_time=$( (time zsh -ilc exit) 2>&1)
  echo "zsh:\t$zsh_elapsed_time"

  # VIM time
  echo "Testing vim"
  vim_elapsed_time=$( (time vim +q) >$(tty))
  echo "vim:\t$vim_elapsed_time"
}

main
