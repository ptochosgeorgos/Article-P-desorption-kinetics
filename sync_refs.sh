#!/bin/bash
set -e

echo "Syncing JabRef changes to the master References repository..."
cd ~/Documents/References
git add references.bib
# Only commit if there are changes
git diff-index --quiet HEAD || git commit -m "Auto-sync: updated references.bib from JabRef"

echo "Pulling changes into the Quarto project submodule..."
cd ~/Documents/Agroscope/Article-P-desorption-kinetics/references_repo
git pull origin master

echo "Done! The manuscript is now synced with your latest JabRef citations."
