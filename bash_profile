# Changin the order of usr/bin and usr/local/bin
# remove /usr/local/bin and /usr/bin
export PATH=`echo ":$PATH:" | sed -e "s#:/usr/local/bin:#:#g" -e "s/^://" -e "s/:$//"`
export PATH=`echo ":$PATH:" | sed -e "s#:/usr/bin:#:#g" -e "s/^://" -e "s/:$//"`
# add /usr/local/bin and /usr/bin in that order
export PATH="/usr/local/bin:/usr/bin:$PATH"

# Adding my scripts folder to $PATH
export PATH=/Users/$USER/scripts:$PATH

# Load ~/.aliases, ~/.functions and ~/.profile
for file in ~/.{aliases,functions,profile}; do
  echo "Sourcing $file"
  [ -r "$file" ] && source "$file"
done
unset file

# Colors for 'ls' and 'grep'
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced
export GREP_OPTIONS='--color=always'
export GREP_COLOR='1;35;40'

# Export my editor of choice!
export EDITOR=vim

# Git branch in prompt.
parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}
export PS1="\[$(tput setaf 3)\]\W\[$(tput setaf 2)\]\$(parse_git_branch) \[$(tput setaf 1)\]\\$ \[$(tput sgr0)\]"

# Don't totally know why this is here.
export VERSIONER_PYTHON_PREFER_32_BIT=yes
