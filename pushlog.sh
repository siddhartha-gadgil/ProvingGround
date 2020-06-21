#!/bin/bash
set -e
cd /home/gadgil/code/ProvingGround
git add logs/* --all
git commit -m "logs updated"
git pull --no-edit
git push
