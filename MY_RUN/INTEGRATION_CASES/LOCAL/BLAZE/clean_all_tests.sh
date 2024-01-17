#!/bin/sh

for test in "01_flat_2WC" "02_flat_A2F" "03_flat_F2A" "04_slope"
do
    cd $test || return 1
    ./clean_test.sh
    cd ..
done