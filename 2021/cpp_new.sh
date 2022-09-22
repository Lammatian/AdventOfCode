#!/bin/bash
DAY=$1

printf -v DAYNUM "%02d" $DAY
mkdir -p "inputs/day${DAYNUM}"
mkdir -p "cpp/day${DAYNUM}"

if [ ! -f "cpp/day${DAYNUM}/main.cpp" ]; then
cat <<EOT > cpp/day${DAYNUM}/main.cpp
#include "../utils.h"

int main()
{
    return 0;
}
EOT
fi

if [ ! -f "inputs/day${DAYNUM}/input" ]; then
    curl "https://adventofcode.com/2021/day/${DAY}/input" --cookie "session=${AOC_SESSION}" > "inputs/day${DAYNUM}/input"
fi

vim cpp/day${DAYNUM}/main.cpp
