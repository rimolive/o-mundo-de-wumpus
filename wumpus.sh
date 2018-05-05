#!/bin/bash
swipl -o mundo-de-wumpus -c src/wumpus.pl src/heuristica.pl src/mundo.pl
./mundo-de-wumpus