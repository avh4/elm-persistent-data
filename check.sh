#!/bin/sh

set -ex

elm-test --yes
elm-make --yes
cd tests
elm-make --yes --debug TestApp.elm --output ../example-server/index.html
