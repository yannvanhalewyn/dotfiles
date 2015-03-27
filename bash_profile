export VERSIONER_PYTHON_PREFER_32_BIT=yes

export CLICOLOR=1

export LSCOLORS=GxFxCxDxBxegedabagaced

export GREP_OPTIONS='--color=always'
export GREP_COLOR='1;35;40'

export PATH=/Users/$USER/bin:/Users/$USER/scripts:$PATH


export PATH="/usr/local/bin:$PATH"

source ~/.profile

# Git branch in prompt.

parse_git_branch() {

    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'

}

# export PS1="\W\[\033[32m\]\$(parse_git_branch)\e[2;31m $ \e[m"
export PS1="\[$(tput setaf 3)\]\W\[$(tput setaf 2)\]\$(parse_git_branch) \[$(tput setaf 1)\]\\$ \[$(tput sgr0)\]"
