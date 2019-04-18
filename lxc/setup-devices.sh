#!/bin/bash

[[ -b /dev/loop-control ]] || mknod -m 0660 /dev/loop-control b 10 237
[[ -b /dev/fuse ]] || mknod -m 666 /dev/fuse c 10 229

for i in $(seq 0 7); do
  [[ -b /dev/loop$i ]] || mknod -m 0660 /dev/loop$i b 7 $i
done
