#!/bin/bash
## created on 2023-02-21

#### enter description here

bwlim=500
rclone="$HOME/PROGRAMS/rclone"
config="$HOME/Documents/rclone.conf"
otheropt=" --checkers=20 --delete-before --stats=300s"
bwlimit="  --bwlimit=${bwlim}k"

if [[ "$(hostname)" = "sagan" ]]; then 
    echo "Upload all pdfs"
    "${rclone}" ${otheropt} ${bwlimit} --config "$config" --include "**/*.pdf"  sync "$HOME/CS_id/" "lapauththanasis:/CS_id_test"
else
    echo "This runs only on sagan"
fi

exit 0 
