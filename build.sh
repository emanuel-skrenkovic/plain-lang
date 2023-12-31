#! /bin/bash

set -e

cargo run test.sg
llc -filetype=obj -O0 -o bin/a.o bin/a.bc
gcc -o bin/a bin/a.o
