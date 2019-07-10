#!/bin/bash

set -euo pipefail

if [ "$EUID" -eq 0 ]; then
  echo This script should not be executed as root
  exit 1
fi

TOUCHPAD_MINIMUM_PRESSURE='0.05'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null && pwd)"
REPO_DIR="$(readlink -f "$SCRIPT_DIR/..")"

# shellcheck source=./functions.sh
. "$SCRIPT_DIR/functions.sh"

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

if [[ -e /home/chronos/user/.nix-profile/etc/profile.d/nix.sh ]]; then
  . /home/chronos/user/.nix-profile/etc/profile.d/nix.sh
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

set +e
read -r -d '' AUGEAS_SCRIPT <<EOF
set etc/gesture/40-touchpad-cmt.conf/InputClass[Identifier = "CMT for Synaptics Touchpad"]/Option[. = "Tap Minimum Pressure"] "Tap Minimum Pressure"
set etc/gesture/40-touchpad-cmt.conf/InputClass[Identifier = "CMT for Synaptics Touchpad"]/Option[. = "Tap Minimum Pressure"]/value $TOUCHPAD_MINIMUM_PRESSURE
EOF
set -e

sudo "$(command -v augtool)" -t 'Xorg incl /etc/gesture/*.conf' "$AUGEAS_SCRIPT"

echo Installing services
sudo "$(command -v rsync)" -azvpu "$REPO_DIR/upstart-services/" /etc/init

sudo cp "$REPO_DIR/bootstrap/30-crosh-custom.sh" /usr/share/crosh/dev.d/
sudo cp "$REPO_DIR/bootstrap/51-android.rules" /etc/udev/rules.d/
sudo cp "$REPO_DIR/bootstrap/99-kvm.rules" /etc/udev/rules.d/

echo Creating system groups
sudo groupadd -f docker
sudo groupadd -f fuse
sudo groupadd -f input
sudo groupadd -f kvm
sudo groupadd -f libvirt
sudo groupadd -f vboxusers
sudo groupadd -f virtaccess

sudo usermod -aG docker,fuse,input,kvm,libvirt,vboxusers,virtaccess "$USER"

if ! [[ -e /etc/sysctl.d/99-custom.conf ]]; then
  echo 'fs.inotify.max_user_watches = 524288' > /etc/sysctl.d/99-custom.conf
fi
