#!/bin/bash

if [ ! -n "$1" ]
then
    echo "Usage: `basename $0` dir-to-store"
    exit 1
fi

dir_to_store=$1

function run {
    echo "Beginning $2 run"

    set -x
    stack exec profiling -- +RTS $1 -p -sprofiling.gc -RTS > profiling.output

    hp2ps -c profiling.hp

    new_dir="${dir_to_store}$2"
    mkdir -p $new_dir
    mv profiling.output $new_dir
    mv profiling.ps $new_dir
    mv profiling.gc $new_dir
    mv profiling.prof $new_dir

    rm -f profiling.{aux,hp}
    set +x
}

stack build --enable-library-profiling --enable-executable-profiling --ghc-options="-auto-all -caf-all -fforce-recomp -rtsopts -O"
run -hc cost-centre
run -hd closure
run -hy type
run -hm module
run -hr retainer

echo "COMPLETE"
