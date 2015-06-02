#!/bin/bash

EXECUTABLE=dist/build/homplexity/homplexity

${EXECUTABLE} --severity=Warning `find . -iname '*.hs'`
