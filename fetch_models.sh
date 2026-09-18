#!/bin/bash
LXC_HOST="root@192.168.1.54"
REMOTE_DIR="/root/workspace"

echo "Pulling currently finished models from the cluster..."
rsync -avzu "$LXC_HOST:$REMOTE_DIR/models/" ./models/
echo "Done! Check your local 'models' folder."
