if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Environment variables
set -gx EDITOR ~/repos/nvim-macos-x86_64/bin/nvim
set -gx BAT_THEME TwoDark

fish_add_path ~/bin

################################################################################
# Aliases

# Utils
alias paths='echo $PATH | tr ":" "\n"'

# Neovim
alias v="~/repos/nvim-macos-x86_64/bin/nvim"
alias v.="v ."
alias vf="v -o (fzf)"
alias vd="d && v."
alias vv="NVIM_APPNAME=nvim-nvchad nvim"

# tmux
alias ta='tmux -u attach'
alias tml='tmux list-sessions'
alias td='tmux-dispensary'
alias :vs='tmux splitw -h'
alias :sp='tmux splitw'
alias :q='tmux kill-pane'

# Git
alias g="git"
alias ga="git add"
alias gaa="git add -A"
alias gb="git branch"
alias gco="git checkout"
alias gcm="git checkout master"
alias gcmsg="git commit -m"
alias gd="git diff"
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

################################################################################
# Function

function killfzf
  set pid (ps aux \
    | fzf \
        --header-lines 1 \
        --preview-window down:80%:wrap \
        --preview "printf '%s\n' {} | awk '{print \$2}' | xargs ps -p" \
    | awk '{print $2}')

  if test -z "$pid"
    echo "No pid selected, exiting."
    return 1
  else
    echo "Killing $pid"
    kill $pid
  end
end

################################################################################
# FZF

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

################################################################################
# Zoxide

if command -v zoxide >/dev/null
    zoxide init --cmd cd fish | source
end

################################################################################
# NVM

# Lazy loading for performance
if test -d "$HOME/.nvm"
  set -gx NVM_DIR "$HOME/.nvm"

  function _load_nvm_and_run
    set cmd $argv[1]

    # Remove all the custom functions
    functions -e nvm node npm npx yarn claude opencode 2>/dev/null

    # Load NVM by sourcing it in a subshell and extracting the PATH
    set -l nvm_output (bash -c "source '$NVM_DIR/nvm.sh' && echo \$PATH")
    set -gx PATH $nvm_output

    # Also set NODE_PATH if it exists
    set -l node_path (bash -c "source '$NVM_DIR/nvm.sh' && echo \$NODE_PATH")
    if test -n "$node_path"
      set -gx NODE_PATH $node_path
    end

    # Run the original command
    command $argv
  end

  # Create functions that will load NVM on first use
  function nvm
    _load_nvm_and_run nvm $argv
  end

  function node
    _load_nvm_and_run node $argv
  end

  function npm
    _load_nvm_and_run npm $argv
  end

  function npx
    _load_nvm_and_run npx $argv
  end

  function yarn
    _load_nvm_and_run yarn $argv
  end

  function claude
    _load_nvm_and_run claude $argv
  end

  function opencode
    _load_nvm_and_run opencode $argv
  end
end
