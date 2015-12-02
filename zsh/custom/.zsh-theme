# Some variables
user_color=blue
machine_color=magenta
path_color=yellow

# =======
# UTILITY
# =======

prompt_segment() {
  local color
  if [[ -n $VIMODE ]]; then
    color="%F{red}"
  else
    [[ -n $2 ]] && color="%F{$2}" || color="%f"
  fi
  echo -n "${color}${1} "
}


# ========
# SEGMENTS
# ========
user() {
  prompt_segment "%n" $user_color
}

machine() {
  prompt_segment "at"
  prompt_segment "%m" $machine_color
}

path() {
  prompt_segment "in"
  prompt_segment "%~" $path_color
}

parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
}

git_branch() {
  local branch=`parse_git_branch`
  if [[ -n $branch ]]; then
    prompt_segment " $(parse_git_branch)" green
  fi
}


prompt() {
  [[ $(jobs -l | wc -l) -gt 0 ]] && prompt_segment "⚙" cyan
  [[ $RETVAL -ne 0 ]] && prompt_segment "✘" red
  prompt_segment "$" green
}

build_prompt() {
  RETVAL=$?
  echo -n "\n"
  user
  machine
  path
  git_branch

  # newline
  echo -n "\n"
  prompt
}

PROMPT='$(build_prompt)'
