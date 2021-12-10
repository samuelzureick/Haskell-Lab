#!/bin/bash
cat submission.hs > check_ex2_temp_file.hs
cat ex2_tests.hs >> check_ex2_temp_file.hs
ghc check_ex2_temp_file.hs
./check_ex2_temp_file
