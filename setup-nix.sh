#!/bin/bash

set -euo pipefail

if [ "$EUID" -eq 0 ]; then
  echo This script should not be executed as root
  exit 1
fi

TOUCHPAD_MINIMUM_PRESSURE='0.05'

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

echo 'Setting the filesystem to mount read/write...'
sudo mount -o remount,rw /
sudo mount -o remount,exec /tmp
sudo mount -o remount,exec /mnt/stateful_partition
sudo mount -i -o remount,exec /home/chronos/user

sudo "$DIR/mount-nix.sh"

if [[ "$(sudo passwd --status chronos | awk '{ print $2 }')" != "P" ]]; then
  echo Defining password for user chronos
  sudo passwd chronos
fi

if ! hash nix-env; then
  curl https://nixos.org/nix/install | sh
  . /home/chronos/user/.nix-profile/etc/profile.d/nix.sh
fi

nix-env -f "<nixpkgs>" -i -A ag
nix-env -f "<nixpkgs>" -i -A augeas
nix-env -f "<nixpkgs>" -i -A bridge-utils
nix-env -f "<nixpkgs>" -i -A curl
nix-env -f "<nixpkgs>" -i -A dnsmasq
nix-env -f "<nixpkgs>" -i -A git
nix-env -f "<nixpkgs>" -i -A gptfdisk
nix-env -f "<nixpkgs>" -i -A homesick
nix-env --set-flag priority 6 homesick
nix-env -f "<nixpkgs>" -i -A htop
nix-env -f "<nixpkgs>" -i -A inetutils
nix-env -f "<nixpkgs>" -i -A less
nix-env -f "<nixpkgs>" -i -A libguestfs
nix-env -f "<nixpkgs>" -i -A lxc
nix-env -f "<nixpkgs>" -i -A lxd
nix-env -f "<nixpkgs>" -i -A man
nix-env --set-flag priority 4 man
nix-env -f "<nixpkgs>" -i -A man-pages
nix-env -f "<nixpkgs>" -i -A nodejs-11_x
nix-env -f "<nixpkgs>" -i -A openssh
nix-env -f "<nixpkgs>" -i -A qemu
nix-env -f "<nixpkgs>" -i -A rclone
nix-env -f "<nixpkgs>" -i -A readline70
nix-env -f "<nixpkgs>" -i -A ruby_2_6
nix-env -f "<nixpkgs>" -i -A socat
nix-env -f "<nixpkgs>" -i -A tig
nix-env -f "<nixpkgs>" -i -A tmux
nix-env -f "<nixpkgs>" -i -A wget
nix-env -f "<nixpkgs>" -i -A xorg.xset
nix-env -f "<nixpkgs>" -i -A zsh

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
sudo "$(which rsync)" -azvpu "$DIR/upstart-services/" /etc/init

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
