function fish_prompt
  set -l last_status $status

  # Colors
  set -l user_color blue
  set -l machine_color magenta
  set -l date_color magenta
  set -l path_color yellow
  set -l vcs_color green
  set -l prompt_color blue

  function prompt_segment
    set -l text $argv[1]
    set -l color $argv[2]

    if test -n "$color"
      set_color $color
    end
    echo -n "$text"
    set_color normal
    echo -n " "
  end

  function parse_git_branch
    git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
  end

  function jj_prompt_info
    # Check if we're in a jj repo
    if not command -v jj >/dev/null 2>&1; or not jj st >/dev/null 2>&1
      return 0
    end

    # Get current change name and ref
    set -l jj_info (jj status --no-pager 2>/dev/null)

    # Extract change ID (name) - look for the working copy line with (@)
    set -l change_line (echo "$jj_info" | grep -E "Working copy.*@" | head -1)
    set -l change_id (echo "$change_line" | awk '{print $3}')
    set -l change_desc (echo "$change_line" | sed 's/.*@): //' | sed 's/^[^ ]* //')

    # Print the formatted result
    echo " ($change_desc)"
  end

  function git_branch
    set -l jj_info (jj_prompt_info)
    if test -n "$jj_info"
      prompt_segment " $jj_info" $vcs_color
    else
      set -l branch (parse_git_branch)
      if test -n "$branch"
        prompt_segment " $branch" $vcs_color
      end
    end
  end

  # First line: path and git info
  echo
  prompt_segment (prompt_pwd) $path_color
  git_branch

  # Second line: status indicators and prompt
  echo
  if test (jobs | wc -l) -gt 0
    prompt_segment "⚙" cyan
  end
  if test $last_status -ne 0
    prompt_segment "✘" red
  end
  prompt_segment "\$" $prompt_color
end
