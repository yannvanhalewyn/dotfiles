# Some variables
user_color=blue
machine_color=magenta
date_color=magenta
path_color=yellow
git_color=green
prompt_color=blue
reset_color=$(tput sgr0)
reset_attrs="\e[0m"

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

datetime() {
  prompt_segment "on"
  prompt_segment "$(date '+%d %b')" $date_color
  prompt_segment "at"
  prompt_segment $(date "+%H:%M") $date_color
}

path() {
  # prompt_segment "in"
  prompt_segment "%~" $path_color
}

parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/' }

git_branch() {
  local branch=`parse_git_branch`
  if [[ -n $branch ]]; then
    prompt_segment " $(parse_git_branch)" $git_color
  fi
}


prompt() {
  [[ $(jobs -l | wc -l) -gt 0 ]] && prompt_segment "⚙" cyan
  [[ $RETVAL -ne 0 ]] && prompt_segment "✘" red
  prompt_segment "$" $prompt_color
}

build_prompt() {
  RETVAL=$?
  echo -n "\n"
  # user
  # datetime
  path
  git_branch

  # newline
  echo -n "\n"
  prompt
  echo -n "$reset_attrs"
}

PROMPT='$(build_prompt)'
