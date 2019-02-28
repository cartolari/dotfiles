#!/bin/bash

[[ -b /dev/loop-control ]] || mknod -m 0660 /dev/loop-control b 10 237

for i in $(seq 0 7); do
  [[ -b /dev/loop$i ]] || mknod -m 0660 /dev/loop$i b 7 $i
done
