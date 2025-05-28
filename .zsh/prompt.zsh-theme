# autoload -U colors && colors
# Package for adding #{$fg[blue]%}something${%reset_color%} to a prompt segment
# Some variables
user_color=blue
machine_color=magenta
date_color=magenta
path_color=yellow
vcs_color=green
prompt_color=blue

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
  # Use %f to reset color instead of raw escape sequence
  echo -n "${color}${1}%f "
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

# I played around with vcs_info package, but I prefered this output and it's
# simpler to integrate with jujutsu
parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
}

jj_prompt_info() {
  # Check if we're in a jj repo
  if ! command -v jj >/dev/null 2>&1 || ! jj st >/dev/null 2>&1; then
    return 0
  fi

  # Get current change name and ref
  local jj_info=$(jj status --no-pager 2>/dev/null)

  # Extract change ID (name)
  local change_line=$(echo "$jj_info" | grep -E "Working copy\s+\(@\)" | head -1)
  local change_id=$(echo "$change_line" | awk '{print $5}')
  local change_desc=$(echo "$change_line" | awk '{for(i=7;i<=NF;i++) printf "%s%s", $i, (i<NF?" ":"")}')

  # Check if working copy has changes
  local dirty=""
  if echo "$jj_info" | grep -q "^M "; then
    # Not really useful?
    # dirty="*"
  fi

  # Print the formatted result
  echo "${change_id} ${change_desc}${dirty}"
}

git_branch() {
  local jj_info=`jj_prompt_info`
  if [[ -n $jj_info ]]; then
    prompt_segment " ${jj_info}" $vcs_color
  else
    local branch=`parse_git_branch`
    if [[ -n $branch ]]; then
      prompt_segment " ${branch}" $vcs_color
    fi
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
  path
  git_branch

  # newline
  echo -n "\n"
  prompt
}

PROMPT='$(build_prompt)'
