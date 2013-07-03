#!/bin/bash

NODE=${1:-"web"}

function release_node {
    rm -rf rels/$1/node/lib
    rm -rf rels/$1/node/data
    rm -rf rels/$1/node/log
    rm -rf rels/$1/node/releases
    cd rels/$1
    rebar -f generate
    cd ../..
    ./nitrogen_static.sh
    ./release_sync.sh
}

if [ "$NODE" == "all" ]; then
   echo "Releasing all nodes..."
   release_node web
else
   echo "Releasing node $NODE..."
   release_node $NODE
fi

