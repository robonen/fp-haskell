#!/usr/bin/env sh

PROG_NAME=$1

if [ -z "$PROG_NAME" ]; then
  echo "Usage: run.sh <program name>"
  exit 1
fi

# Source and build paths
SRC_PATH=src/$PROG_NAME/$PROG_NAME.hs

if [ ! -f $SRC_PATH ]; then
  echo "File $SRC_PATH does not exist"
  exit 1
fi

ghci $SRC_PATH