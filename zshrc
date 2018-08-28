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

source /usr/local/share/antigen/antigen.zsh
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle git
antigen apply
source ~/.zsh/custom/.zsh-theme

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

export EDITOR='vim'
source "$HOME/.aliases"
source "$HOME/.functions"

bindkey '\e[A' history-beginning-search-backward
bindkey '\e[B' history-beginning-search-forward

# auto push dirs on stack
setopt autopushd

# GPG agent for prompting passphrase
export GPG_TTY=$(tty)

# grep colors
export GREP_OPTIONS='--color=always'

# Disable bracketed paste feature when in emacs. This used to cause
# unwanted chars to be printed out
# https://github.com/syl20bnr/spacemacs/issues/3035
if [ -n "$INSIDE_EMACS" ]; then
    export EDITOR=emacsclient
    unset zle_bracketed_paste  # This line
fi
# =====
# RBENV
# =====
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
eval "$(direnv hook zsh)"
