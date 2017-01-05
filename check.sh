#!/bin/sh

set -ex

elm-test
elm-make
cd tests
elm-make --debug TestApp.elm --output TestApp.html
