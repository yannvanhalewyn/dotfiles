if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Environment variables
# set -gx EDITOR nvim
# set -gx BAT_THEME TwoDark

# FZF configuration
set -gx FZF_DEFAULT_COMMAND 'fd --type f --hidden --follow --exclude .git'

# FZF options for Ctrl+T (file search)
set -gx FZF_CTRL_T_OPTS "--height 60% \
--border sharp \
--layout reverse \
--prompt '∷ ' \
--pointer ▶ \
--marker ⇒ \
--preview 'bat -n --color=always --line-range :500 {}'"

# FZF options for Alt+C (directory search)
set -gx FZF_ALT_C_OPTS "--preview 'eza --tree --color=always {} | head -200'"

# Initialize FZF key bindings
if command -v fzf >/dev/null
    fzf --fish | source
end

# Zoxide (smart cd)
if command -v zoxide >/dev/null
    zoxide init --cmd cd fish | source
end

# Neovim
alias v="nvim"
alias v.="v ."
alias vf="v -o (fzf)"
alias vd="d && v."

# Git
alias g="git"
alias ga="git add"
alias gaa="git add -A"
alias gb="git branch"
alias gco="git checkout"
alias gcm="git checkout master"
alias gd="git difftool"
alias gdc="git difftool --cached"
alias gf="git fetch"
alias gp="git push"
alias gpf="git push --force-with-lease"
alias glog="git log --graph --oneline"
alias gs="git status"
alias gsha="git rev-parse HEAD"
alias gsta="git stash"
alias gstl="git stash list"
alias gstp="git stash pop"
alias gwhip="git commit -am 'WIP'"
alias gunwhip="git reset HEAD~1 --soft && git reset HEAD"
alias gamend="git commit --amend --no-edit"
alias grhh="git reset HEAD --hard"

# Jujutsu
alias j="jj log -n 5"
alias jbl="jj bookmark list"
alias jd="jj describe"
alias jgf="jj git fetch"
alias jgp="jj git push --tracked"
alias jj_pull="jj git fetch && jj bookmark set master -r master@origin"
alias je="jj edit"
alias jl="jj log -n 10"
alias jll="jj log -r ::"
alias jlm="jj log -r master-..@"
alias jn="jj new"
alias jna="jj new -A"
alias jnaa="jj new -A @"
alias jr="jj rebase"
alias jrh="jj rebase -h"
alias js="jj squash"
alias jsr="jj squash -r"
alias jsi="jj split -i"
alias jst="jj status"
alias jsh="jj show"

# Docker
alias dup="docker-compose up -d"
