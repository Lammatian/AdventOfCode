#!/bin/bash
DAY=$1
printf -v DAYNUM "%02d" $DAY
if [ -z "$2" ]; then
    python3 "src/day${DAYNUM}/main.py"
else
    python3 "src/day${DAYNUM}/main.py" $2
fi
