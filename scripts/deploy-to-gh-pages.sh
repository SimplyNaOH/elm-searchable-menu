#!/bin/bash

# Adapted from
# https://github.com/thebritican/elm-autocomplete/blob/master/scripts/deploy-to-gh-pages.sh

set -e

git checkout gh-pages
git pull origin gh-pages
git merge master --no-edit
cd examples
elm make src/Example.elm --output=main.js
cp index.html main.js ../
cp css/app.css ../css/
cd ..
git add index.html main.js css/app.css examples/main.js
git commit -m 'Update gh-pages files'
git push origin gh-pages
git checkout master
