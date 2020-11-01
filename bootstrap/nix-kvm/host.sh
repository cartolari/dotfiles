#!/bin/bash

set -euo pipefail

if [ "$EUID" -eq 0 ]; then
  echo This script should not be executed as root
  exit 1
fi

export LD_LIBRARY_PATH=
export VM_DISK_FOLDER="$HOME/vms/"
export VM_DISK="$HOME/vms/dev-vm.qcow2"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null && pwd)"
REPO_DIR="$(readlink -f "$SCRIPT_DIR/../..")"

echo 'Setting the filesystem to mount read/write...'
sudo mount -o remount,rw /
sudo mount -o remount,exec /tmp
sudo mount -o remount,exec /mnt/stateful_partition
sudo mount -i -o remount,exec /home/chronos/user

if [[ "$(sudo passwd --status chronos | awk '{ print $2 }')" != "P" ]]; then
  echo Defining password for user chronos
  sudo passwd chronos
fi

sudo mkdir -p /nix
mkdir -p /home/chronos/user/nix
if ! mountpoint /nix &> /dev/null; then
  sudo mount --bind /home/chronos/user/nix /nix
fi

if [[ -e ~/.nix-profile/etc/profile.d/nix.sh ]]; then
  . ~/.nix-profile/etc/profile.d/nix.sh
fi

source ~/.bashrc
hash -r

if ! hash nix-env; then
  curl -L https://nixos.org/nix/install | sh
  ~/.nix-profile/etc/profile.d/nix.sh
fi

echo Installing services
sudo cp "$REPO_DIR"/bootstrap/common/upstart-services/* /etc/init
sudo cp "$REPO_DIR"/bootstrap/nix-kvm/upstart-services/* /etc/init

echo Installing packages
nix-env -f '<nixpkgs>' -iA bitwarden-cli
nix-env -f '<nixpkgs>' -iA bridge-utils
nix-env -f '<nixpkgs>' -iA dnsmasq
nix-env -f '<nixpkgs>' -iA expect
nix-env -f '<nixpkgs>' -iA gptfdisk
nix-env -f '<nixpkgs>' -iA jq
nix-env -f '<nixpkgs>' -iA oathToolkit
nix-env -f '<nixpkgs>' -iA openfortivpn
nix-env -f '<nixpkgs>' -iA openvpn
nix-env -f '<nixpkgs>' -iA qemu
nix-env -f '<nixpkgs>' -iA socat
nix-env -f '<nixpkgs>' -iA telnet

echo Copying scripts
sudo cp "$SCRIPT_DIR"/host-bin/* /usr/local/bin

echo Setup the VM
mkdir -p "$VM_DISK_FOLDER"
sudo cp "$SCRIPT_DIR"/qemu-ifup /etc/qemu-ifup
if [[ ! -e "$VM_DISK" ]]; then
  cd ~/Downloads
  wget https://vagrantcloud.com/generic/boxes/arch/versions/3.0.28/providers/libvirt.box
  tar xf ~/Downloads/libvirt.box -C ~/vms box.img
  mv ~/vms/box.img "$VM_DISK"
fi
