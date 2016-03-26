#!/bin/sh

# Creates network from scratch, with songs provided in $@ dir

set +e

for f in $@/*
do
  stack exec music-fun-datagen < $f
done
