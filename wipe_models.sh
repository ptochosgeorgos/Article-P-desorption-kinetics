#!/bin/bash
LXC_HOST="root@192.168.1.54"
REMOTE_DIR="/root/workspace"
ssh $LXC_HOST "rm -f $REMOTE_DIR/models/*.rds"
echo "Old corrupted model caches have been wiped from the cluster."
