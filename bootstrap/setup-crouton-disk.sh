#!/bin/bash

if [ "$EUID" -ne 0  ]; then
  echo This script should be executed as root
  exit 1
fi

set -euo pipefail

export LD_LIBRARY_PATH=/usr/local/lib64:/usr/local/lib
export PATH=$PATH:/usr/local/sbin:/usr/local/bin

CROUTON_IMG_FILE=/mnt/stateful_partition/code.img
CROUTON_KEY_FILE=/home/chronos/user/.crouton-keyfile
CROUTON_MOUNT_POINT=/mnt/stateful_partition/crouton

prompt_confirmation() {
  local question=$1
  read -r -p "$question [y/N] " response

  if [[ ! "$response" =~ ^([yY][eE][sS]|[yY])+$ ]]; then
    echo Cannot continue
    exit 1
  fi
}

if [[ ! -e $CROUTON_IMG_FILE ]]; then
  prompt_confirmation "File $CROUTON_IMG_FILE doesn't exist create it?"
  truncate -s 35G "$CROUTON_IMG_FILE"
fi

if [[ ! -e $CROUTON_KEY_FILE ]]; then
  prompt_confirmation "File $CROUTON_KEY_FILE doesn't exist create it?"
  dd if=/dev/urandom of="$CROUTON_KEY_FILE" bs=1024 count=4
fi

if ! "$(command -v cryptsetup)" -v luksOpen --test-passphrase --key-file "$CROUTON_KEY_FILE" "$CROUTON_IMG_FILE"; then
  echo Could not detect a dm-crypt partition in $CROUTON_IMG_FILE. Create it?
  "$(command -v cryptsetup)" -v \
    --cipher aes-xts-plain64 \
    --hash sha256 \
    --iter-time 3000 \
    --key-file "$CROUTON_KEY_FILE" \
    --key-size 512 \
    --use-random \
    luksFormat \
    "$CROUTON_IMG_FILE"

  echo Created Encrypted partition. To add a password independent of the key file run:
  echo cryptsetup luksAddKey --key-file "$CROUTON_KEY_FILE" "$CROUTON_IMG_FILE"
fi

if [[ ! -e /dev/mapper/crouton ]]; then
  "$(command -v cryptsetup)" luksOpen --key-file "$CROUTON_KEY_FILE" "$CROUTON_IMG_FILE" crouton
fi

if ! file -sL /dev/mapper/crouton | grep ext4; then
  prompt_confirmation No ext4 filesystem detected on $CROUTON_IMG_FILE. Create one?
  mkfs.ext4 /dev/mapper/crouton
fi

mkdir -p "$CROUTON_MOUNT_POINT"
if ! mountpoint "$CROUTON_MOUNT_POINT" > /dev/null; then
  mount /dev/mapper/crouton "$CROUTON_MOUNT_POINT"
fi

mkdir -p "$CROUTON_MOUNT_POINT/shared/code"
