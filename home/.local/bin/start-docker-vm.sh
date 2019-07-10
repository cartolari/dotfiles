#!/bin/bash

if [ "$EUID" -eq 0  ]; then
  echo This script should not be executed as root
  exit 1
fi

sudo qemu-system-x86_64 \
  -cdrom $HOME/.local/share/docker-vm/seed.iso \
  -cpu host \
  -device virtio-balloon \
  -device virtio-net-pci,netdev=mynet0,mac=52:54:00:D3:EC:C7 \
  -device virtio-rng-pci \
  -device virtio-serial \
  -display vnc=:5 \
  -drive file=$HOME/.local/share/docker-vm/disk.img,if=virtio,aio=threads,format=qcow2 \
  -enable-kvm \
  -m 8192 \
  -netdev tap,id=mynet0,script=$HOME/.homesick/repos/chromebook-dotfiles/qemu-ifup \
  -nographic \
  -smp "$(nproc)" \
  -vga virtio
