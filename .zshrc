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
antigen bundle zsh-users/zsh-autosuggestions
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

export EDITOR='nvim'
source "$HOME/.aliases"
source "$HOME/.functions"

bindkey '\e[A' history-beginning-search-backward
bindkey '\e[B' history-beginning-search-forward

# auto push dirs on stack
setopt autopushd

# GPG agent for prompting passphrase
export GPG_TTY=$(tty)

# man colors
export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'

# Man pages with progress percentage
export MANPAGER='less -s -M +Gg'

export PATH="$HOME/bin:/usr/local/opt/grep/libexec/gnubin:/usr/local/opt/coreutils/libexec/gnubin:$PATH"
export PATH="/Applications/SuperCollider.app/Contents/Resources:$PATH"
export PATH="$HOME/repos/elasticsearch-8.6.2/bin:$PATH"
export PATH="$HOME/repos/kibana-8.6.2/bin:$PATH"

################################################################################
# RBENV

if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
if which direnv > /dev/null; then eval "$(direnv hook zsh)"; fi

################################################################################
# JAVA

export JAVA_8_HOME=$(/usr/libexec/java_home -v1.8)
export JAVA_11_HOME=$(/usr/libexec/java_home -v11)
export JAVA_16_HOME=$(/usr/libexec/java_home -v16)
export JAVA_17_HOME=$(/usr/libexec/java_home -v17)
export JAVA_HOME=$JAVA_17_HOME
alias java8=' export JAVA_HOME=$JAVA_8_HOME'
alias java11='export JAVA_HOME=$JAVA_11_HOME'
alias java16='export JAVA_HOME=$JAVA_16_HOME'
alias java17='export JAVA_HOME=$JAVA_17_HOME'

################################################################################
# Rust
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"

################################################################################
# Android SDK

export ANDROID_SDK="$HOME/Library/Android/sdk"
export ANDROID_HOME="$HOME/Library/Android/sdk"
export ANDROID_AVD_HOME=~/.android/avd
export PATH="$ANDROID_SDK/platform-tools:$PATH"
export PATH="$ANDROID_SDK/tools:$PATH"
export PATH="$ANDROID_SDK/tools/bin:$PATH"
export PATH="$ANDROID_SDK/emulator/:$PATH"

################################################################################
# CLIs

export PATH="$HOME/.emacs.d/bin:$PATH"     # Doom cli

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

_bb_tasks() {
    local matches=(`bb tasks | tail -n +3 | cut -f1 -d ' '`)
    compadd -a matches
    _files # autocomplete filenames as well
}
compdef _bb_tasks bb

# export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
