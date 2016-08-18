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

# =======
# Antigen
# =======

source "$HOME/.antigen/antigen.zsh"
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle git
antigen bundle rails
antigen bundle akoenig/gulp-autocompletion-zsh
antigen-theme "$HOME/.zsh/custom"
antigen-apply

setopt PROMPT_SUBST
setopt MENU_COMPLETE
setopt extendedglob
# Jump to first possible completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
# Highlight selected completion in suggestions list
zstyle ':completion:*' menu select

# ========
# History
# ========
HISTFILE=$HOME/.zhistory       # enable history saving on shell exit
setopt APPEND_HISTORY          # append rather than overwrite history file.
HISTSIZE=20000                 # lines of history to maintain memory
SAVEHIST=20000                 # lines of history to maintain in history file.

export EDITOR='nvim'
source "$HOME/.aliases"
source "$HOME/.functions"

bindkey '\e[A' history-beginning-search-backward
bindkey '\e[B' history-beginning-search-forward

# auto push dirs on stack
setopt autopushd

# Add scripts to path
export PATH="$PATH:$HOME/.scripts"

# =====
# RBENV
# =====
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
