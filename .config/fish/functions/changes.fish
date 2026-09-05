function changes --description "Pick a recent change (jj/git) and diff with difftastic"
    argparse --name=changes 'n/nvim' 'h/help' -- $argv
    or return

    if set -q _flag_help
        echo "Usage: changes [--nvim|-n]"
        echo ""
        echo "  Browse recent jj (or git) changes with fzf preview,"
        echo "  then open the selected change in difftastic."
        echo "  Returns to the picker after the diff viewer exits."
        echo ""
        echo "  Navigation: j/k or arrow keys. q or Esc to quit."
        echo ""
        echo "  --nvim, -n   Open in Neovim with :Difft <id> instead"
        return 0
    end

    set -l vcs
    if jj root --quiet 2>/dev/null
        set vcs jj
    else if git rev-parse --show-toplevel >/dev/null 2>&1
        set vcs git
    else
        echo "changes: not inside a jj or git repository" >&2
        return 1
    end

    set -l fzf_opts --ansi \
        --delimiter='\t' --nth=2.. --tabstop=10 \
        --preview-window='right,60%,border-left,wrap' \
        --height=80% --layout=reverse --border=sharp

    set -l selected
    switch $vcs
        case jj
            set selected (jj log --no-graph -n 50 \
                -T 'change_id.shortest(8) ++ "\t" ++ if(bookmarks, bookmarks.join(" ") ++ " ", "") ++ if(description, description.first_line(), "(no description)") ++ "\n"' \
                | fzf $fzf_opts \
                    --prompt='[jj] change > ' \
                    --preview 'jj diff --stat --color=always -r {1}')
        case git
            set -l commits (git log -n 50 --color=always --pretty=format:'%h%x09%C(auto)%d %s')
            git diff --quiet; or set -p commits 'HEAD\t(unstaged changes)'
            set selected (printf '%s\n' $commits \
                | fzf $fzf_opts \
                    --prompt='[git] change > ' \
                    --preview '[ {1} = HEAD ] && git diff --stat --color=always || git show --format= --stat --color=always {1}')
    end

    test -n "$selected"; or return

    set -l id (string split -m1 \t -- $selected)[1]

    if set -q _flag_nvim
        if test $id = HEAD
            nvim -c "Difft"
        else
            nvim -c "Difft $id"
        end
    else
        switch $vcs
            case jj
                jj diff --tool difft -r $id
            case git
                if test $id = HEAD
                    GIT_EXTERNAL_DIFF=difft git diff
                else
                    GIT_EXTERNAL_DIFF=difft git show --ext-diff $id
                end
        end
    end
end
