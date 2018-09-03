#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

modprobe nbd max_part=8
[[ -b /dev/nbd0p1 ]] ||
  qemu-nbd --connect=/dev/nbd0 "$DIR/arch.qcow2"

if ! mountpoint "$LXC_ROOTFS_PATH"; then
  mount /dev/nbd0p1 "$LXC_ROOTFS_PATH"
fi
