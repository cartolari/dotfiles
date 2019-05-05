#!/bin/bash

set -euo pipefail

CONTAINERS=/home/chronos/user/containers
LD_LIBRARY_PATH=

export LD_LIBRARY_PATH

prompt_confirmation() {
  local question=$1

  echo $question
  read -r -p "$question [y/N] " response

  if [[ ! "$response" =~ ^([yY][eE][sS]|[yY])+$ ]]; then
    echo Cannot continue
    exit 1
  fi
}

if [[ $EUID -ne 0 ]]; then
 echo "This script should be run as root" 1>&2
 exit 1
fi

mkdir -p "$CONTAINERS/rootfs"
cd "$CONTAINERS"
chown chronos:chronos rootfs

if [[ ! -f arch.img ]]; then
  prompt_confirmation "File $CONTAINERS/arch.img does not exist. Create it?"
  touch arch.img
  chown chronos:chronos arch.img

  for i in seq 1 10; do
    time dd if=/dev/zero bs=1M count=5000 | pv -s 5G -L 150M >> "$CONTAINERS/arch.img"
    sleep 10
    time sync
    sleep 40
  done

  time sync
fi

if ! file arch.img | grep -i ext4 > /dev/null; then
  prompt_confirmation "File $CONTAINERS/arch.img does not have an EXT4 filesystem. Create it?"
  mkfs.ext4 arch.img
fi

if ! mountpoint rootfs > /dev/null; then
  mount arch.img rootfs/
fi

cd rootfs/

if [[ ! -f etc/resolv.conf ]]; then
  rm -rf etc/resolv.conf
  touch etc/resolv.conf
fi
mount -o ro --bind /etc/resolv.conf etc/resolv.conf
mount -t proc /proc proc/
mount --rbind /sys sys/
mount --rbind /dev dev/
