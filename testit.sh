#!/bin/bash

EXECUTABLE=dist/build/homplexity/homplexity

${EXECUTABLE} --severity=Info `find . -iname '*.hs'`
