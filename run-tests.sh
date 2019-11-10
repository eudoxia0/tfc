#!/usr/bin/env bash

make tfc
./tfc test/not2.f > test/not2.ll
llc test/not2.ll
