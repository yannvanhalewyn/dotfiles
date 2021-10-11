#
# 8 888888888o   8 888888888o.      ,o888888o.     8 8888888888    8 8888 8 8888         8 8888888888
# 8 8888    `88. 8 8888    `88.  . 8888     `88.   8 8888          8 8888 8 8888         8 8888
# 8 8888     `88 8 8888     `88 ,8 8888       `8b  8 8888          8 8888 8 8888         8 8888
# 8 8888     ,88 8 8888     ,88 88 8888        `8b 8 8888          8 8888 8 8888         8 8888
# 8 8888.   ,88' 8 8888.   ,88' 88 8888         88 8 888888888888  8 8888 8 8888         8 888888888888
# 8 888888888P'  8 888888888P'  88 8888         88 8 8888          8 8888 8 8888         8 8888
# 8 8888         8 8888`8b      88 8888        ,8P 8 8888          8 8888 8 8888         8 8888
# 8 8888         8 8888 `8b.    `8 8888       ,8P  8 8888          8 8888 8 8888         8 8888
# 8 8888         8 8888   `8b.   ` 8888     ,88'   8 8888          8 8888 8 8888         8 8888
# 8 8888         8 8888     `88.    `8888888P'     8 8888          8 8888 8 888888888888 8 888888888888

# Changin the order of usr/bin and usr/local/bin
# remove /usr/local/bin and /usr/bin
export PATH=`echo ":$PATH:" | sed -e "s#:/usr/local/bin:#:#g" -e "s/^://" -e "s/:$//"`
export PATH=`echo ":$PATH:" | sed -e "s#:/usr/bin:#:#g" -e "s/^://" -e "s/:$//"`
# add ~/bin /usr/local/bin and /usr/bin in that order
export PATH="$HOME/bin:/usr/local/bin:/usr/bin:$PATH"

# # Load ~/.aliases, ~/.functions and ~/.profile, ~/.bash_prompt
# for file in ~/.{aliases,functions,profile,bash_prompt}; do
#   [ -r "$file" ] && source "$file"
# done
# unset file

# Colors for 'ls' and 'grep'
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced
export GREP_OPTIONS='--color=always'
export GREP_COLOR='1;35;40'

# Export my editor of choice!
export EDITOR=vim

# True vim colors in neovim
export NVIM_TUI_ENABLE_TRUE_COLOR=1

export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
