#!/bin/sh

cd 01_prep_ideal_case || return 1
./clean_test
cd ../02_mesonh || return 1
./clean_test -a

cd ..