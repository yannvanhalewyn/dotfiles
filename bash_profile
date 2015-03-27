# Load ~/.extra, ~/.bash_prompt, ~/.exports, ~/.aliases and ~/.functions
# ~/.extra can be used for settings you don’t want to commit
for file in ~/.{extra,bash_prompt,exports,aliases,functions}; do
[ -r "$file" ] && source "$file"
done
unset file

# bash completion.
if [ -f $(brew --prefix)/share/bash-completion/bash_completion ]; then
. $(brew --prefix)/share/bash-completion/bash_completion
fi



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
