#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

if mountpoint "$LXC_ROOTFS_PATH"; then
  umount "$LXC_ROOTFS_PATH"
fi

[[ -b /dev/nbd0p1 ]] && qemu-nbd --diconnect /dev/nbd0
