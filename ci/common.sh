#!/bin/bash

message () {
  echo -e "\e[1m\e[33m${*}\e[0m"
}
set -o verbose,xtrace,pipefail

