#!/bin/bash
set -e

# Builds the test workflow binary and makes a .alfredworkflow from the skeleton

stack build

file=$(stack path --local-install-root)
file+="/bin/alfred-hoogle"

cp -f "$file" "./workflow_skeleton/alfred-hoogle"
zip -r -X -j Hoogle.alfredworkflow workflow_skeleton


