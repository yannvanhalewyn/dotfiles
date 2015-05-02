# Changin the order of usr/bin and usr/local/bin
# remove /usr/local/bin and /usr/bin
export PATH=`echo ":$PATH:" | sed -e "s#:/usr/local/bin:#:#g" -e "s/^://" -e "s/:$//"`
export PATH=`echo ":$PATH:" | sed -e "s#:/usr/bin:#:#g" -e "s/^://" -e "s/:$//"`
# add /usr/local/bin and /usr/bin in that order
export PATH="/usr/local/bin:/usr/bin:$PATH"

# Adding my scripts folder to $PATH
export PATH=/Users/$USER/scripts:/usr/local/sbin:$PATH

# Load ~/.aliases, ~/.functions and ~/.profile, ~/.bash_prompt
for file in ~/.{aliases,functions,profile,bash_prompt}; do
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

# Don't totally know why this is here.
export VERSIONER_PYTHON_PREFER_32_BIT=yes

# Bash completion
if [ -f $(brew --prefix)/share/bash-completion/bash_completion ]; then
  . $(brew --prefix)/share/bash-completion/bash_completion
fi
