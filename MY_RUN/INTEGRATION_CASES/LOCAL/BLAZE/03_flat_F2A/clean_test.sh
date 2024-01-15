#!/bin/sh

cd 01_prep_ideal_case || return 1
./clean_test
cd ../02_mesonh || return 1
./clean_test -d
rm -rf BlazeTest.des BlazeTest.nc PGDBlazeTest.des PGDBlazeTest.nc

cd ..
