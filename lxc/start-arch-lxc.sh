#!/bin/bash

set -xeuo pipefail
IFS=$'\n\t'

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
CONTAINERS=/home/chronos/user/containers
LXC_PREFIX=/home/chronos/user/.nix-profile

export LD_LIBRARY_PATH=
export PATH=$LXC_PREFIX/bin:$PATH

if [[ $EUID -ne 0 ]]; then
 echo "This script must be run as root" 1>&2
 exit 1
fi

prompt_confirmation() {
  local question=$1

  echo $question
  read -r -p "$question [y/N] " response

  if [[ ! "$response" =~ ^([yY][eE][sS]|[yY])+$ ]]; then
    echo Cannot continue
    exit 1
  fi
}

# Network
if [[ ! -f /etc/default/lxc-net ]]; then
  echo Creating /etc/default/lxc-net
  cat <<EOF > /etc/default/lxc-net
USE_LXC_BRIDGE="true"
LXC_BRIDGE="lxcbr0"
LXC_ADDR="10.0.3.1"
LXC_NETMASK="255.255.255.0"
LXC_NETWORK="10.0.3.0/24"
EOF
else
  echo Using existing /etc/default/lxc-net
fi

if grep CHECKSUM $LXC_PREFIX/libexec/lxc/lxc-net; then
  # There is no easy way to get iptables extensions in ChromiumOS
  echo 'Removing CHECKSUM from iptables rules'
  sed -i '/-j CHECKSUM/d' $LXC_PREFIX/libexec/lxc/lxc-net
fi

if ! ip a | grep lxcbr0 > /dev/null; then
  $LXC_PREFIX/libexec/lxc/lxc-net start
fi

# Storage
mkdir -p "$CONTAINERS"

if [[ ! -f "$CONTAINERS/arch.img" ]]; then
  prompt_confirmation "File $CONTAINERS/arch.img does not exist. Create it?"

  for i in seq 1 10; do
    time dd if=/dev/zero bs=1M count=5000 | pv -s 5G -L 150M >> "$CONTAINERS/arch.img"
    sleep 10
    time sync
    sleep 40
  done
fi
chown -R chronos:chronos /home/chronos/user/containers

time sync

if ! file /home/chronos/user/containers/arch.img | grep -i ext4 2> /dev/null; then
  prompt_confirmation "File $CONTAINERS/arch.img does not have an EXT4 filesystem. Create it?"
  mkfs.ext4 /home/chronos/user/containers/arch.img
fi

mkdir -p /usr/local/var/lib/lxc/arch/rootfs
if ! mountpoint /usr/local/var/lib/lxc/arch/rootfs; then
  mount /home/chronos/user/containers/arch.img /usr/local/var/lib/lxc/arch/rootfs
fi

# Container creation
if ! lxc-ls -f | grep -E '\barch\b' > /dev/null; then
  echo Creating LXC Container

  lxc-create \
    -t download \
    -n arch \
    -- \
    --dist archlinux \
    --release current \
    --arch amd64 
else
  echo LXC container is created
fi

lxc-stop arch || true

. $DIR/mount-cgroups.sh

# Container startup
if lxc-ls -f | grep -E '\barch\b' | grep STOPPED > /dev/null; then
  rsync -azvp "$DIR/arch/" /usr/local/var/lib/lxc/arch/
  mkdir -p /var/lib/lxc/rootfs
  lxc-start -n arch -l debug --logfile /dev/stdout
fi
