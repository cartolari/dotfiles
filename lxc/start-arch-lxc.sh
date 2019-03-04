#!/bin/bash

set -xeuo pipefail
IFS=$'\n\t'

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

if [[ $EUID -ne 0 ]]; then
 echo "This script must be run as root" 1>&2
 exit 1
fi

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

modprobe nbd max_part=8
[[ -f /home/chronos/user/containers/arch.qcow2 ]] ||
  qemu-img create -f qcow2 /home/chronos/user/containers/arch.qcow2 30G
[[ -e /dev/nbd0p1 ]] ||
  qemu-nbd -c /dev/nbd0 -f qcow2 /home/chronos/user/containers/arch.qcow2

if [[ ! -e /dev/nbd0p1 ]]; then
  sgdisk -n 1:0: /dev/nbd0
  mkfs.ext4 /dev/nbd0p1
fi

mkdir -p /usr/local/var/lib/lxc/arch/rootfs
if ! mountpoint /usr/local/var/lib/lxc/arch/rootfs; then
  mount /dev/nbd0p1 /usr/local/var/lib/lxc/arch/rootfs
fi

# Container creation
if ! lxc-ls -f | grep -E '\barch\b' > /dev/null; then
  echo Creating LXC Container

  # Override PATH so lxc-create use Chromebrew sed instead of system
  env PATH=/usr/local/bin:$PATH lxc-create \
    -t download \
    -n arch \
    -- \
    --dist archlinux \
    --release current \
    --arch amd64 
else
  echo LXC container is created
fi

. $DIR/mount-cgroups.sh

# Container startup
if lxc-ls -f | grep -E '\barch\b' | grep STOPPED > /dev/null; then
  cat <<EOF > /usr/local/var/lib/lxc/arch/config
# lxc.include = /usr/local/share/lxc/config/nesting.conf

# Distribution configuration
lxc.include = /usr/local/share/lxc/config/common.conf
lxc.include = /usr/local/share/lxc/config/nesting.conf
lxc.arch = x86_64

lxc.cap.drop =

# Container specific configuration
lxc.rootfs.path = dir:/usr/local/var/lib/lxc/arch/rootfs
lxc.uts.name = arch

lxc.cgroup.devices.allow = a
lxc.mount.auto = proc:rw sys:rw cgroup-full:mixed:force

# Network configuration
lxc.net.0.type = veth
lxc.net.0.flags = up
lxc.net.0.link = lxcbr0
lxc.net.0.ipv4.address = 10.0.3.2/24
lxc.net.0.ipv4.gateway = 10.0.3.1

lxc.mount.entry=/home/chronos/user/Downloads/ home/bruno/Downloads none bind,optional,create=dir
lxc.mount.entry=/run/chrome/ opt/chrome none bind,optional,create=dir 0 0
lxc.mount.entry=/home/chronos/user/.homesick/repos/chromebook-dotfiles/lxc/ usr/local/lxc-scripts none bind,optional,create=dir 0 0

lxc.mount.entry = /dev/dri dev/dri none bind,optional,create=dir
lxc.mount.entry = /dev/snd dev/snd none bind,optional,create=dir
lxc.mount.entry = /dev/video0 dev/video0 none bind,optional,create=file

lxc.mount.entry = /home/chronos/user/.homesick/repos/chromebook-dotfiles/lxc-systemd-services/mount-loop-devices.service etc/systemd/system/mount-loop-devices.service none bind,optional,create=file
EOF
  lxc-start -n arch -l debug --logfile /dev/stdout
fi
