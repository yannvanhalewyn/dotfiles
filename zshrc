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
plugins=(git npm gem rvm)

source $ZSH/oh-my-zsh.sh

# ============
# CUSTOM SETUP
# ============

# Load ~/.aliases and ~/.functions
for file in ~/.{aliases,functions}; do
  [ -r "$file" ] && source "$file"
done
unset file

export EDITOR='nvim'

# True vim colors in neovim
export NVIM_TUI_ENABLE_TRUE_COLOR=1

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

