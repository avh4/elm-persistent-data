#!/bin/bash

set -ex

./check.sh
(cd examples && elm-make --yes SimpleApp.elm --output ../docs/SimpleApp.elm.html)
