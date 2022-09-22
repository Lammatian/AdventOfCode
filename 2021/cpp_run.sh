#!/bin/bash
DAY=$1
printf -v DAYNUM "%02d" $DAY
mkdir -p cpp/day${DAYNUM}/build
g++ --std=c++17 cpp/day${DAYNUM}/main.cpp -o cpp/day${DAYNUM}/build/main && ./cpp/day${DAYNUM}/build/main < inputs/day${DAYNUM}/${2:-input}
