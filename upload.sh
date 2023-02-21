#!/bin/bash
## created on 2023-02-21

#### enter description here

echo "i am $0 who the hell are you"
TIC=$(date +"%s")
## start coding


bwlim=500
rclone="$HOME/PROGRAMS/rclone"
config="$HOME/Documents/rclone.conf"
otheropt=" --checkers=20 --delete-before --stats=300s"
bwlimit="  --bwlimit=${bwlim}k"


"${rclone}" ${otheropt} ${bwlimit} --config "$config" copy "$HOME/CS_id/**/*.pdf" "lapauththanasis:/CS_id_test"




exit 0 
