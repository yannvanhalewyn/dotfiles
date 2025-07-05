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

# Load Completions
autoload -Uz compinit && compinit

# Load plugins
source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source $(brew --prefix)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

################################################################################
# FZF Tab

# This section just needs to source fzf-tab, but also checks if it's installed
# and installs it automatically if it's not for new machines.
# Consider using zinit, see https://www.youtube.com/watch?v=ud7YxC33Z3w
FZF_TAB_PLUGIN_ROOT="$HOME/repos/fzf-tab"
FZF_TAB_PLUGIN="$FZF_TAB_PLUGIN_ROOT/fzf-tab.plugin.zsh"

if [[ ! -f "$FZF_TAB_PLUGIN" ]]; then
    echo "fzf-tab plugin not found. Installing..."
    mkdir -p "$HOME/repos"
    git clone https://github.com/Aloxaf/fzf-tab "$FZF_TAB_PLUGIN_ROOT"

    if [[ $? -eq 0 ]]; then
      echo "fzf-tab plugin successfully installed!"
    else
      echo "Error: Failed to install fzf-tab plugin."
    fi
fi
source "$FZF_TAB_PLUGIN"

################################################################################

source ~/.zsh/prompt.zsh-theme

setopt prompt_subst
setopt MENU_COMPLETE
setopt extendedglob

# Jump to first possible completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
# Highlight selected completion in suggestions list
zstyle ':completion:*' menu no
# zstyle ':fzf-tab:complete:cd:*' fzf-preview 'eza -1 --color=always --group-directories-first --icons'
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls --color $realpath'

# ========
# History
# ========
# HISTFILE=$HOME/.zhistory       # enable history saving on shell exit
HISTSIZE=20000                   # lines of history to maintain memory
SAVEHIST=20000                   # lines of history to maintain in history file.
setopt appendhistory             # append rather than overwrite history file.
setopt hist_ignore_space         # ignore commands that start with a space, used for sensitive commands
setopt hist_ignore_dups          # ignore duplicated commands
setopt hist_ignore_all_dups      # ignore duplicated commands
setopt hist_save_no_dups         # don't save duplicated commands

bindkey '^p' history-search-backward
bindkey '^n' history-search-forward


export EDITOR='nvim'
source "$HOME/.aliases"
source "$HOME/.functions"

# bindkey '\e[A' history-beginning-search-backward
# bindkey '\e[B' history-beginning-search-forward

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

################################################################################
# RBENV

# Disable rbenv while I don't use ruby
# if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
# if which direnv > /dev/null; then eval "$(direnv hook zsh)"; fi

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

# Not working yet but interesting
# https://pragmaticpineapple.com/four-useful-fzf-tricks-for-your-terminal/
_fzf_comprun() {
  local command=$1
  shift

  case "$command" in
    cd)           fzf "$@" --preview 'tree -L 1 -C {} | head -200' ;;
    *)            fzf "$@" --preview 'bat --color=always --line-range :500 {}';;
  esac
}

# Bat
export BAT_THEME='TwoDark'

# https://thevaluable.dev/fzf-shell-integration/
export FZF_CTRL_T_OPTS="--height 60% \
--border sharp \
--layout reverse \
--prompt '∷ ' \
--pointer ▶ \
--marker ⇒
--preview 'bat -n --color=always --line-range :500 {}'"

export FZF_ALT_C_OPTS="--preview 'eza --tree --color=always {} | head -200'"


# Zoxide
eval "$(zoxide init --cmd cd zsh)"

# NVM
# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
# [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Lazily load nvm
if [ -s "$HOME/.nvm/nvm.sh" ]; then
  export NVM_DIR="$HOME/.nvm"
  [ -s "$NVM_DIR/bash_completion" ] && . "$NVM_DIR/bash_completion"

  alias nvm='unalias nvm node npm yarn claude && . "$NVM_DIR"/nvm.sh && nvm'
  alias node='unalias nvm node npm yarn claude && . "$NVM_DIR"/nvm.sh && node'
  alias npm='unalias nvm node npm yarn claude && . "$NVM_DIR"/nvm.sh && npm'
  alias yarn='unalias nvm node npm yarn claude && . "$NVM_DIR"/nvm.sh && yarn'
  alias claude='unalias nvm node npm yarn claude && . "$NVM_DIR"/nvm.sh && claude'
fi

#################################################################################
# Babashka

_bb_tasks() {
    local matches=(`bb tasks |tail -n +3 |cut -f1 -d ' '`)
    compadd -a matches
    _files # autocomplete filenames as well
}
compdef _bb_tasks bb
source <(jj util completion zsh)
compdef _jj jj

# Fixes strange codes in `gt log` timeline segment symbols
export LANG=en_US.UTF-8

#################################################################################
# Graphite

#compdef gt
###-begin-gt-completions-###
#
# yargs command completion script
#
# Installation: gt completion >> ~/.zshrc
#    or gt completion >> ~/.zprofile on OSX.
#
_gt_yargs_completions()
{
  local reply
  local si=$IFS
  IFS=$'
' reply=($(COMP_CWORD="$((CURRENT-1))" COMP_LINE="$BUFFER" COMP_POINT="$CURSOR" gt --get-yargs-completions "${words[@]}"))
  IFS=$si
  _describe 'values' reply
}
compdef _gt_yargs_completions gt
###-end-gt-completions-###
export PATH="/opt/homebrew/bin:$PATH"
