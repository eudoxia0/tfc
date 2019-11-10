#!/usr/bin/env bash

make tfc
./tfc test/not2.f > test/not2.ll
llc -filetype=obj test/not2.ll -o test/not2.o
