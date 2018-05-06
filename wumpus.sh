#!/bin/bash
swipl -o mundo-de-wumpus -c src/wumpus.pl $1 src/mundo.pl
./mundo-de-wumpus