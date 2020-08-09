#!/bin/bash

set -euo pipefail

if [ "$EUID" -eq 0 ]; then
  echo This script should not be executed as root
  exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null && pwd)"
REPO_DIR="$(readlink -f "$SCRIPT_DIR/..")"

export LD_LIBRARY_PATH=/usr/local/lib64:/usr/local/lib
export PATH=$PATH:/usr/local/sbin:/usr/local/bin

if [[ ! -f /usr/local/bin/crew ]]; then
  curl -Ls git.io/vddgY | bash
fi

crew install cryptsetup
crew install git iptables xzutils # Docker deps

echo 'Setting the filesystem to mount read/write...'
sudo mount -o remount,rw /
sudo mount -o remount,exec /tmp
sudo mount -o remount,exec /mnt/stateful_partition
sudo mount -i -o remount,exec /home/chronos/user

if [[ "$(sudo passwd --status chronos | awk '{ print $2 }')" != "P" ]]; then
  echo Defining password for user chronos
  sudo passwd chronos
fi

echo Installing services
sudo cp $REPO_DIR/upstart-services/* /etc/init

sudo cp "$REPO_DIR/bootstrap/30-crosh-custom.sh" /usr/share/crosh/dev.d/
sudo cp "$REPO_DIR/bootstrap/51-android.rules" /etc/udev/rules.d/
sudo cp "$REPO_DIR/bootstrap/99-kvm.rules" /etc/udev/rules.d/

if ! [[ -e /etc/sysctl.d/99-custom.conf ]]; then
  echo 'fs.inotify.max_user_watches = 524288' | sudo tee /etc/sysctl.d/99-custom.conf
fi

echo Installing Docker
if [[ ! -f /usr/local/bin/dockerd ]]; then
  curl -SsL https://download.docker.com/linux/static/stable/x86_64/docker-17.09.0-ce.tgz | sudo tar xzf - -C /usr/local/bin --strip-components=1
fi

sudo "$SCRIPT_DIR/setup-crouton-disk.sh"
sudo cp "$SCRIPT_DIR/docker.json" /mnt/stateful_partition/docker.json
