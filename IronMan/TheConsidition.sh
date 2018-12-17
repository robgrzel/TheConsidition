#!/usr/bin/env bash
set -x

LINK_PATH=$(readlink ${0})
LINK_DIR=${LINK_PATH%/*}
FNAME=$(basename -- "$0")
FDIR=$(cd -P -- "$(dirname -- "$0")" && pwd -P)
FPATH=$FDIR/$FNAME

if [[ -n "$LINK_DIR" ]]; then
    echo "We are pointed by sym link..."
    echo "...LINK_PATH = $LINK_PATH"
    echo "...LINK_DIR = $LINK_DIR"
    cd $LINK_DIR
    MYDIR=$LINK_DIR
elif [[ -n "$FDIR" ]]; then
    echo "We are in file $FPATH"
    echo "...FPATH = $FPATH"
    echo "...FDIR = $FDIR"
    MYDIR=$FDIR
fi

ls



#g++ Cpp/opencv.img.scaling.cpp -std=c++17 -o cmake-build-debug/TestOpenCV -lncurses $(pkg-config opencv --cflags --libs)

#cd cmake-build-debug
#./TestOpenCV
#exit

#set -e
#g++ CppTests/fast_hash.cpp -std=c++17 -o cmake-build-debug/app -lncurses
#./cmake-build-debug/app $MYDIR


VISIBLEDISTANCE=25

set -e
g++ Cpp/mapdraw.cpp -Wall -std=c++17 -o cmake-build-debug/app -lncurses -funroll-loops
./cmake-build-debug/app $MYDIR ${VISIBLEDISTANCE}


