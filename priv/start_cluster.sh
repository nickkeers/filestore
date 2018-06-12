#!/bin/bash


export RELX_REPLACE_OS_VARS=true

for i in `seq 1 $1`;
do
    echo Starting node es3_$i
    NODE_NAME=es3_${i} PORT_START=${i}0000 REST_PORT=8${i}00 _build/default/rel/filestore$i/bin/filestore$i foreground &
    sleep 1
done