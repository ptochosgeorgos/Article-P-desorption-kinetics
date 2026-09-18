#!/bin/bash
unset LD_PRELOAD
unset DBUS_SESSION_BUS_ADDRESS

if [ -z "$1" ]; then
    echo "Usage: ./crunch.sh <r_script_name.R | script.qmd>"
    exit 1
fi

SCRIPT_NAME=$1
LXC_HOST="root@192.168.1.54"
REMOTE_DIR="/root/workspace"

echo "[1/3] Syncing workspace to LXC container ($LXC_HOST)..."
ssh $LXC_HOST "mkdir -p $REMOTE_DIR"
rsync -avz --exclude '.git' --exclude '.Rhistory' --exclude 'crunch.sh' ./ "$LXC_HOST:$REMOTE_DIR/"

echo "[2/3] Executing $SCRIPT_NAME on LXC container..."
if [[ "$SCRIPT_NAME" == *.qmd ]]; then
    ssh $LXC_HOST "cd $REMOTE_DIR && quarto render $SCRIPT_NAME"
else
    ssh $LXC_HOST "cd $REMOTE_DIR && Rscript $SCRIPT_NAME"
fi

echo "[3/3] Pulling results back to local workspace..."
rsync -avzu "$LXC_HOST:$REMOTE_DIR/" ./

echo "Done!"
