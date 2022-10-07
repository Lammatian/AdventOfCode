#!/bin/bash
DAY=$1
printf -v DAYNUM "%02d" $DAY
python3 "src/day${DAYNUM}/main.py"
