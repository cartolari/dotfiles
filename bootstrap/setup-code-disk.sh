#!/bin/bash

if [ "$EUID" -ne 0  ]; then
  echo This script should be executed as root
  exit 1
fi

set -euo pipefail

CODE_IMG_FILE=/var/lib/crouton-shared/docker/docker.img
CODE_KEY_FILE="$HOME/.docker-keyfile"

prompt_confirmation() {
  local question=$1

  read -r -p "$question [y/N] " response

  if [[ ! "$response" =~ ^([yY][eE][sS]|[yY])+$ ]]; then
    echo Cannot continue
    exit 1
  fi
}

if [[ ! -e $CODE_IMG_FILE ]]; then
  prompt_confirmation "File $CODE_IMG_FILE doesn't exist create it?"
  sudo truncate -s 35G "$CODE_IMG_FILE"
fi

if [[ ! -e $CODE_KEY_FILE ]]; then
  prompt_confirmation "File $CODE_KEY_FILE doesn't exist create it?"
  dd if=/dev/urandom of="$CODE_KEY_FILE" bs=1024 count=4
fi

if ! sudo "$(command -v cryptsetup)" -v luksOpen --test-passphrase --key-file "$CODE_KEY_FILE" "$CODE_IMG_FILE"; then
  echo Could not detect a dm-crypt partition in $CODE_IMG_FILE. Create it?
  sudo "$(command -v cryptsetup)" -v \
    --cipher aes-xts-plain64 \
    --hash sha256 \
    --iter-time 3000 \
    --key-file "$CODE_KEY_FILE" \
    --key-size 512 \
    --use-random \
    luksFormat \
    "$CODE_IMG_FILE"

  echo Created Encrypted partition. To add a password independent of the key file run:
  echo sudo cryptsetup luksAddKey --key-file "$CODE_KEY_FILE" "$CODE_IMG_FILE"
fi

if [[ ! -e /dev/mapper/code ]]; then
  sudo "$(command -v cryptsetup)" luksOpen --key-file "$CODE_KEY_FILE" "$CODE_IMG_FILE" code
fi

if ! sudo file -sL /dev/mapper/code | grep ext4; then
  prompt_confirmation No ext4 filesystem detected on $CODE_IMG_FILE. Create one?
  sudo mkfs.ext4 /dev/mapper/code
fi

mkdir -p /home/cartolari/code
chown cartolari:cartolari /home/cartolari/code
if ! mountpoint /var/lib/code > /dev/null; then
  sudo mount /dev/mapper/code /home/cartolari/code
fi
