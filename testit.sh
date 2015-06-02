#!/bin/bash

EXECUTABLE=dist/build/homplexity/homplexity

${EXECUTABLE} `find . -iname '*.hs'`
