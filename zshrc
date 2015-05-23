#
#  8888888888',8888' d888888o.   8 8888        8 8 888888888o.      ,o888888o.
#         ,8',8888'.`8888:' `88. 8 8888        8 8 8888    `88.    8888     `88.
#        ,8',8888' 8.`8888.   Y8 8 8888        8 8 8888     `88 ,8 8888       `8.
#       ,8',8888'  `8.`8888.     8 8888        8 8 8888     ,88 88 8888
#      ,8',8888'    `8.`8888.    8 8888        8 8 8888.   ,88' 88 8888
#     ,8',8888'      `8.`8888.   8 8888        8 8 888888888P'  88 8888
#    ,8',8888'        `8.`8888.  8 8888888888888 8 8888`8b      88 8888
#   ,8',8888'     8b   `8.`8888. 8 8888        8 8 8888 `8b.    `8 8888       .8'
#  ,8',8888'      `8b.  ;8.`8888 8 8888        8 8 8888   `8b.     8888     ,88'
# ,8',8888888888888`Y8888P ,88P' 8 8888        8 8 8888     `88.    `8888888P'

# ===============
# OH-MY-ZSH SETUP
# ===============

export ZSH=/Users/yannvanhalewyn/.oh-my-zsh

ZSH_THEME="agnoster2" # cool one is jonathan, mira, agnoster

ENABLE_CORRECTION="true"

COMPLETION_WAITING_DOTS="true"

ZSH_CUSTOM=/Users/yannvanhalewyn/.zsh

# very useful was grunt, scans the gruntfile and shows all
# available commands with description
# last-working-dir is also nice
plugins=(git npm gem rvm vi-mode)

source $ZSH/oh-my-zsh.sh

# ============
# CUSTOM SETUP
# ============
bindkey '\e[A' history-beginning-search-backward
bindkey '\e[B' history-beginning-search-forward

# Load ~/.aliases and ~/.functions
for file in ~/.{aliases,functions}; do
  [ -r "$file" ] && source "$file"
done
unset file

export EDITOR='nvim'

# ====
# PATH
# ====

# Changin the order of usr/bin and usr/local/bin
# remove /usr/local/bin and /usr/bin
export PATH=`echo ":$PATH:" | sed -e "s#:/usr/local/bin:#:#g" -e "s/^://" -e "s/:$//"`
export PATH=`echo ":$PATH:" | sed -e "s#:/usr/bin:#:#g" -e "s/^://" -e "s/:$//"`
# add /usr/local/bin and /usr/bin in that order
export PATH="/usr/local/bin:/usr/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
export PATH="$HOME/scripts:$PATH"
