#!/bin/bash
echo "1. Syncing project to LXC..."
rsync -avz --exclude '.git' --exclude 'docs' --exclude '.quarto' /home/marc/Documents/Agroscope/Article-P-desorption-kinetics/ root@192.168.1.54:/root/Article-P-desorption-kinetics/

echo "2. Rendering on LXC..."
ssh root@192.168.1.54 'cd /root/Article-P-desorption-kinetics && quarto render notebooks/qi_modelling_parallel.qmd'

echo "3. Pulling _freeze back to host..."
rsync -avz root@192.168.1.54:/root/Article-P-desorption-kinetics/_freeze/ /home/marc/Documents/Agroscope/Article-P-desorption-kinetics/_freeze/

echo "DONE."
