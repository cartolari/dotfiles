#!/usr/bin/env bash

pac=$(checkupdates | wc -l)
aur=$(cower -u | wc -l)

echo "$((pac + aur))  "
