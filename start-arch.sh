#!/bin/bash

sudo env LD_LIBRARY_PATH=/usr/lib/:/usr/lib64:/usr/local/lib/:/usr/local/lib64/ \
  /usr/local/bin/qemu-system-x86_64 \
  -chardev socket,path=/tmp/qga.sock,server,nowait,id=qga0 \
  -cpu host \
  -daemonize \
  -display vnc=:5 \
  -device virtio-balloon \
  -device virtio-net-pci,netdev=net0,mac=DE:AD:BE:EF:EB:4C \
  -device virtio-serial \
  -device virtserialport,chardev=qga0,name=org.qemu.guest_agent.0 \
  -drive file=/home/chronos/user/vms/archlinux.img,if=virtio \
  -enable-kvm \
  -m 6144 \
  -netdev tap,id=net0,script=/usr/local/etc/qemu-ifup,downscript=no \
  -qmp unix:/home/chronos/user/vms/qmp.sock,server,nowait \
  -smp 2 \
  -vga virtio \
  -virtfs local,path=/home/chronos/user/Downloads,mount_tag=downloads,security_model=passthrough,id=downloads
