#!/bin/bash

################ Make sure only 1 or 2 arguments are passed in ################
if [[ $# <  1 ]]; then
    echo "error: at least 1 argument expected"
    echo "useage: ./graphene.sh path/to/code/program.gph [-b binary] "
    exit 1
fi
if [[ $# >  2 ]]; then
    echo "error: at most 2 arguments expected"
    echo "useage: ./graphene.sh path/to/code/program.gph [-b binary] "
    exit 1
fi
###############################################################################

if [[ $# ==  1 ]]; then
    # Ensure Graphene file is passed in
    if [[ ${1: -4} != ".gph" ]]; then 
        echo "error: unrecognized argument"
        echo "useage: ./graphene.sh path/to/code/program.gph [-b binary] "
        exit 1
    fi

    ./graphene.native $1 > program.ll
    llc -relocation-model=pic program.ll > program.s
    cc -o program.exe program.s graphene.o
    rm program.ll program.s
    ./program.exe
fi

if [[ $# ==  2 ]]; then
    # Ensure Graphene file is passed in
    if [[ ${1: -4} != ".gph" ]]; then 
        echo "error: unrecognized argument"
        echo "useage: ./graphene.sh path/to/code/program.gph [-b binary] "
        exit 1
    fi
    # Ensure proper argument is passed in for only compiling but not running
    if [[ ${2} != "--only-compile" ]]; then 
        echo "error: unrecognized argument"
        echo "useage: ./graphene.sh path/to/code/program.gph [-b binary] "
        exit 1
    fi

    ./graphene.native $1 > program.ll
    llc -relocation-model=pic program.ll > program.s
    cc -o program.exe program.s graphene.o
    rm program.ll program.s
fi