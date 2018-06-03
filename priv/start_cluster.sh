#!/bin/bash


export RELX_REPLACE_OS_VARS=true

for i in `seq 1 $1`;
do
    NODE_NAME=es3_$i PORT_START=2$i000 NUM_NODES=$1 _build/default/rel/filestore/bin/filestore$i foreground &
    sleep 1
done