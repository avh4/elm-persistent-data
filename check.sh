#!/bin/sh

set -ex

elm-test --yes
elm-make --yes
cd examples
elm-make --yes --debug TestAppWithCache.elm --output ../example-server/index.html
