#!/bin/sh

set -ex

elm-test --yes
elm-make --yes

pushd examples
elm-make --yes --debug TestAppWithCache.elm --output ../example-server/index.html
popd
