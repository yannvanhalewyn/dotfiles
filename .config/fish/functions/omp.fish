function omp --description 'omp; under ~/mediquest it runs inside the Mediquest sandbox'
    if string match -q -- "$HOME/mediquest/*" "$PWD/"
        ~/mediquest/home-repo/dev/mq-omp $argv
    else
        command omp $argv
    end
end
