#!/bin/bash

set -uo pipefail
IFS=$'\n\t'

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

if [[ $EUID -ne 0 ]]; then
 echo "This script must be run as root" 1>&2
 exit 1
fi

lxc-stop arch
umount /usr/local/var/lib/lxc/arch/rootfs/
qemu-nbd -d /dev/nbd0
