#!/bin/bash
set -e

LXC_HOST="root@192.168.1.54"
PROJECT_DIR="/root/Article-P-desorption-kinetics"
LOCAL_DIR="/home/marc/Documents/Agroscope/Article-P-desorption-kinetics"

echo "=========================================="
echo "1. Syncing project to LXC container ($LXC_HOST)"
echo "=========================================="
rsync -avz --exclude '.git' --exclude 'docs' --exclude '.quarto' "$LOCAL_DIR/" "$LXC_HOST:$PROJECT_DIR/"

echo "=========================================="
echo "2. Running Bayesian Sampling on LXC (Detached Background Job)"
echo "=========================================="
# Using nohup to execute the R code in the background on the container
ssh "$LXC_HOST" "cd $PROJECT_DIR && nohup quarto render notebooks/bayesian_modelling.qmd > brms_run.log 2>&1 &"

echo "=========================================="
echo "DONE: The job has been submitted to the LXC container!"
echo "It is now running safely in the background. If your local machine restarts"
echo "or the SSH connection drops, the job will continue to run."
echo ""
echo "To check the live logs, run:"
echo "ssh $LXC_HOST \"tail -f $PROJECT_DIR/brms_run.log\""
echo "=========================================="
