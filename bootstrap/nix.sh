#!/bin/bash

set -euo pipefail

if [ "$EUID" -eq 0 ]; then
  echo This script should not be executed as root
  exit 1
fi

TOUCHPAD_MINIMUM_PRESSURE='0.05'

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
REPO_DIR="$(readlink -f "$SCRIPT_DIR/..")"

echo 'Setting the filesystem to mount read/write...'
sudo mount -o remount,rw /
sudo mount -o remount,exec /tmp
sudo mount -o remount,exec /mnt/stateful_partition
sudo mount -i -o remount,exec /home/chronos/user

if [[ "$(sudo passwd --status chronos | awk '{ print $2 }')" != "P" ]]; then
  echo Defining password for user chronos
  sudo passwd chronos
fi

mkdir -p /nix
mkdir -p /home/chronos/user/nix
if ! mountpoint /nix &> /dev/null; then
  sudo mount --bind /home/chronos/user/nix /nix
fi

source ~/.bashrc

hash -r

if ! hash nix-env; then
  curl https://nixos.org/nix/install | sh
  . /home/chronos/user/.nix-profile/etc/profile.d/nix.sh
fi

mkdir -p ~/.config/nixpkgs
[[ -e ~/.config/nixpkgs/config.nix ]] ||
  ln -s "$REPO_DIR/home/.config/nixpkgs/config.nix" ~/.config/nixpkgs/config.nix

nix-env -f "<nixpkgs>" -iA myPackages

echo Add power manager overrides
# 4 hours
echo '14400000' | sudo tee /var/lib/power_manager/plugged_suspend_ms
sudo chown -R power:power /var/lib/power_manager/
sudo restart powerd

if [[ ! -f /etc/zsh/zshrc ]]; then
  sudo mkdir -p /etc/zsh
  sudo "$(which wget)" -O /etc/zsh/zshrc "https://git.grml.org/?p=grml-etc-core.git;a=blob_plain;f=etc/zsh/zshrc;hb=HEAD"
fi

set +e
read -r -d '' AUGEAS_SCRIPT <<EOF
set etc/gesture/40-touchpad-cmt.conf/InputClass[Identifier = "CMT for Synaptics Touchpad"]/Option[. = "Tap Minimum Pressure"] "Tap Minimum Pressure"
set etc/gesture/40-touchpad-cmt.conf/InputClass[Identifier = "CMT for Synaptics Touchpad"]/Option[. = "Tap Minimum Pressure"]/value $TOUCHPAD_MINIMUM_PRESSURE
EOF
set -e

sudo "$(which augtool)" -t 'Xorg incl /etc/gesture/*.conf' "$AUGEAS_SCRIPT"

echo Installing services
sudo "$(which rsync)" -azvpu "$REPO_DIR/upstart-services/" /etc/init

download_initial_vm_disk() {
  if [[ -f /home/chronos/user/vms/archlinux.img ]]; then
    return
  fi

  # Get the previous fifth day of the month which is when
  # ArchLinux images are released on VagrantCloud
  local closest_fifth_day="$(date -d '4 days ago' +%Y.%m).05"
  mkdir -p /home/chronos/user/vms
  cd /home/chronos/user/Downloads
  [[ -f libvirt.box ]] ||
    wget "https://vagrantcloud.com/archlinux/boxes/archlinux/versions/$closest_fifth_day/providers/libvirt.box"
  tar xf libvirt.box box.img
  mv box.img /home/chronos/user/vms/archlinux.img
  qemu-img resize /home/chronos/user/vms/archlinux.img 60G
}

echo Installing initial ArchLinux VM disk
download_initial_vm_disk
