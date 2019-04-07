#!/bin/bash

if [[ $EUID -ne 0 ]]; then
  echo This script must be run as root 1>&2
  exit 1
fi

mkdir -p /nix
mkdir -p /home/chronos/user/nix
chown chronos:chronos /nix

if ! mountpoint /nix &> /dev/null; then
  mount --bind /home/chronos/user/nix /nix
fi

