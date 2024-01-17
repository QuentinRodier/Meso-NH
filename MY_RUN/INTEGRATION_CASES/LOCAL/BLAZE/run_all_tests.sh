#!/bin/sh

for test in "01_flat_2WC" "02_flat_A2F" "03_flat_F2A"
do
    cd $test || return 1
    ./run_test.sh
    cd ..
done
