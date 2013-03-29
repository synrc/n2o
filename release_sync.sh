#!/bin/bash

wd=`pwd`
appss=( "rels/web/node/lib/web-1" )

declare -A sources
declare -A apps

for src in `ls -d apps/*`; do 
    app=(`echo $src | tr '/' ' '`)
    sources[${app[1]}]="1"
done

for dir in ${appss[@]} ; do
    lib=(`echo $dir | tr '-' ' ' | tr '/' ' '`)
    ap=${lib[4]}
    if [ "${sources[$ap]}" = "1" ]; then
        apps[$dir]="apps/$ap"
    fi
done

for key in ${!apps[@]}; do 
    echo "release sync processing $key -> ${apps[$key]}";
    rm -rf "$key/ebin"
    rm -rf "$key/include"
    rm -rf "$key/priv"
    if [ -d "${apps[$key]}/ebin" ]; then
         ln -s  "$wd/${apps[$key]}/ebin" "$wd/$key/ebin"
    fi
    if [ -d "${apps[$key]}/include" ]; then
         ln -s  "$wd/${apps[$key]}/include" "$wd/$key/include"
    fi
    if [ -d "${apps[$key]}/priv" ]; then
         ln -s  "$wd/${apps[$key]}/priv" "$wd/$key/priv"
    fi
done
