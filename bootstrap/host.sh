#!/bin/bash

set -euo pipefail

if [ "$EUID" -eq 0 ]; then
  echo This script should not be executed as root
  exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null && pwd)"
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

echo Add power manager overrides
# 4 hours
echo '14400000' | sudo tee /var/lib/power_manager/plugged_suspend_ms
sudo chown -R power:power /var/lib/power_manager/
sudo restart powerd

echo Installing services
sudo "$(command -v rsync)" -azvpu "$REPO_DIR/upstart-services/" /etc/init

sudo cp "$REPO_DIR/bootstrap/30-crosh-custom.sh" /usr/share/crosh/dev.d/
sudo cp "$REPO_DIR/bootstrap/51-android.rules" /etc/udev/rules.d/
sudo cp "$REPO_DIR/bootstrap/99-kvm.rules" /etc/udev/rules.d/

if ! [[ -e /etc/sysctl.d/99-custom.conf ]]; then
  echo 'fs.inotify.max_user_watches = 524288' > /etc/sysctl.d/99-custom.conf
fi

if [[ ! -e ~/.crouton-password ]]; then
  echo New Crouton password generated in ~/.crouton-password
  openssl rand 32 -hex > ~/.crouton-password
  cat ~/.crouton-password
fi
