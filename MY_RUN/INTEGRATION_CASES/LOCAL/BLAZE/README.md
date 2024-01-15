README KTEST BLAZE FIRE MODEL
=============================

version of MesoNH: 5.5.1
pyrolib version: 0.3.0
Update date: 23/08/2022

This folder contains test cases for MesoNH-Blaze model.
It covers most of the use cases but it will not cover 100% of possibilities as the number of test case should be limited.
Test cases are focus on user usage with default numerical settings.
Development tests with a particular focus on numerical schemes, code robustness and error handling are not considered here.

# List of test cases

1. Flat terrain, two-way coupled
2. Flat terrain, forced by the atmosphere one way coupled
3. Flat terrain, fire replay coupling mode
4. Slope test
5. Parallel test

# Usage

Each test case can be run in its directory by running the script `run_test.sh`.
The script `run_all_tests.sh` run every test sequentially. 
Cleaning scripts are also available.