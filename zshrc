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
antigen bundle vi-mode
antigen-theme "$HOME/.zsh/agnoster"
antigen-apply
setopt PROMPT_SUBST

# ========
# History
# ========
HISTFILE=$HOME/.zhistory       # enable history saving on shell exit
setopt APPEND_HISTORY          # append rather than overwrite history file.
HISTSIZE=1200                  # lines of history to maintain memory
SAVEHIST=1000                  # lines of history to maintain in history file.

export EDITOR='nvim'
source "$HOME/.aliases"
source "$HOME/.functions"

bindkey '\e[A' history-beginning-search-backward
bindkey '\e[B' history-beginning-search-forward

# ===========================
# Helper to check for vi-mode
# ===========================
VIMODE=
function zle-keymap-select {
 VIMODE="${${KEYMAP/vicmd/1}/(main|viins)/}"
 zle reset-prompt
}
zle -N zle-keymap-select
