#!/bin/bash

set -euo pipefail
IFS=$'\n\t'

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

# Network
if ! grep -E '^USE_LXC_BRIDGE="true"' /usr/local/etc/default/lxc > /dev/null; then
  echo Enabling LXC bridge
  sed -i 's/USE_LXC_BRIDGE="false"/USE_LXC_BRIDGE="true"/' /usr/local/etc/default/lxc
  cat /usr/local/etc/default/lxc
else
  echo LXC bridge is enabled
fi

if grep CHECKSUM /usr/local/libexec/lxc/lxc-net; then
  # There is no easy way to get iptables extensions in ChromiumOS
  echo 'Removing CHECKSUM from iptables rules'
  sed -i '/-j CHECKSUM/d' /usr/local/libexec/lxc/lxc-net
fi

[[ -d /usr/local/var/lib/misc/ ]] || mkdir -p /usr/local/var/lib/misc/

if ! ip a | grep lxcbr0 > /dev/null; then
  /usr/local/libexec/lxc/lxc-net start
fi

# Storage
[[ -d /home/chronos/user/containers ]] || mkdir -p /home/chronos/user/containers
chown -R chronos:chronos /home/chronos/user/containers

create_disk() {
  qemu-img create -f qcow2 /home/chronos/user/containers/arch.qcow2 30G
  modprobe nbd max_part=8
  qemu-nbd --connect=/dev/nbd0 /home/chronos/user/containers/arch.qcow2
  sgdisk -n 1:0: /dev/nbd0
  mkfs.ext4 /dev/nbd0p1
}

mount_disk() {
  mkdir -p /usr/local/var/lib/lxc/arch/rootfs
  mount /dev/nbd0p1 /usr/local/var/lib/lxc/arch/rootfs
}

# Container creation
if ! lxc-ls -f | grep -E '\barch\b' > /dev/null; then
  echo Creating LXC Container
  create_disk
  mount_disk

  # Override PATH so lxc-create use Chromebrew sed instead of system
  env PATH=/usr/local/bin:$PATH lxc-create \
    -t download \
    -n arch \
    -- \
    --dist archlinux \
    --release current \
    --arch amd64 
else
  modprobe nbd max_part=8
  [[ -b /dev/nbd0p1 ]] ||
    qemu-nbd --connect=/dev/nbd0 /home/chronos/user/containers/arch.qcow2
  mount_disk
  echo LXC container is created
fi

. $DIR/mount-cgroups.sh

# Container startup
if lxc-ls -f | grep -E '\barch\b' | grep STOPPED > /dev/null; then
  cat <<EOF > /usr/local/var/lib/lxc/arch/config
# lxc.include = /usr/local/share/lxc/config/nesting.conf

# Distribution configuration
lxc.include = /usr/local/share/lxc/config/common.conf
lxc.arch = x86_64

# Container specific configuration
lxc.rootfs.path = dir:/usr/local/var/lib/lxc/arch/rootfs
lxc.uts.name = arch

lxc.cgroup.devices.allow = a

# Network configuration
# lxc.net.0.type = empty
lxc.net.0.type = veth
lxc.net.0.flags = up
lxc.net.0.link = lxcbr0
lxc.net.0.ipv4.address = 10.0.3.2/24
lxc.net.0.ipv4.gateway = 10.0.3.1

lxc.mount.entry=/home/chronos/user/Downloads/ home/bruno/Downloads none bind,optional,create=dir
lxc.mount.entry=/run/chrome/ opt/chrome none bind,optional,create=dir 0 0
EOF
  lxc-start -n arch -l debug
fi
