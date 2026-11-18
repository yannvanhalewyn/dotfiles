# Babashka completions: tasks from bb.edn plus built-in subcommands.

function __bb_tasks
    # `bb tasks` prints a 2-line header, then "name  docstring" rows.
    bb tasks 2>/dev/null | tail -n +3 | string replace -r '^(\S+)\s*(.*)$' '$1\t$2'
end

function __bb_no_subcommand
    set -l tokens (commandline -opc)
    test (count $tokens) -eq 1
end

complete -c bb -f
complete -c bb -n __bb_no_subcommand -a '(__bb_tasks)'

complete -c bb -n __bb_no_subcommand -a tasks -d 'List tasks in bb.edn'
complete -c bb -n __bb_no_subcommand -a run -d 'Run a task'
complete -c bb -n __bb_no_subcommand -a repl -d 'Start a REPL'
complete -c bb -n __bb_no_subcommand -a nrepl-server -d 'Start an nREPL server'
complete -c bb -n __bb_no_subcommand -a socket-repl -d 'Start a socket REPL'
complete -c bb -n __bb_no_subcommand -a prepare -d 'Download deps and pods'
complete -c bb -n __bb_no_subcommand -a print-deps -d 'Print deps.edn-style deps'
complete -c bb -n __bb_no_subcommand -a uberscript -d 'Collect namespaces into one script'
complete -c bb -n __bb_no_subcommand -a uberjar -d 'Build an uberjar'
complete -c bb -n __bb_no_subcommand -a version -d 'Print version'
complete -c bb -n __bb_no_subcommand -a help -d 'Print help'

complete -c bb -n '__fish_seen_subcommand_from run' -a '(__bb_tasks)'

# Files are fine after these flags or as a script argument.
complete -c bb -s f -l file -rF -d 'Run file'
complete -c bb -s e -l eval -r -d 'Evaluate expression'
complete -c bb -s m -l main -r -d 'Call -main of namespace'
complete -c bb -s x -l exec -r -d 'Call function with CLI args'
complete -c bb -l config -rF -d 'Use alternate bb.edn'
complete -c bb -l deps-root -rF -d 'Deps root directory'
complete -c bb -l classpath -r -d 'Classpath'
complete -c bb -s h -l help -d 'Print help'
complete -c bb -n __bb_no_subcommand -a '(__fish_complete_suffix .clj .bb .cljc)'
