#!/usr/bin/env sh

PROG_NAME=$1

if [ -z "$PROG_NAME" ]; then
  echo "Usage: run.sh <program name>"
  exit 1
fi

# Source and build paths
SRC_PATH=src/$PROG_NAME/$PROG_NAME.hs
BUILD_PATH=build/$PROG_NAME

# Build trash paths
OBJ_PATH=src/$PROG_NAME/$PROG_NAME.o
HI_PATH=src/$PROG_NAME/$PROG_NAME.hi

if [ ! -f $SRC_PATH ]; then
  echo "File $SRC_PATH does not exist"
  exit 1
fi

ghc -o $BUILD_PATH $SRC_PATH \
  && rm $OBJ_PATH $HI_PATH \
  && ./$BUILD_PATH