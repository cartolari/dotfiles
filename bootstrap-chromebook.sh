#!/bin/bash

set -euo pipefail

BRIDGE_UTILS_VERSION=1.6
CHROMEBREW_PACKAGES='chromebrew_scripts gptfdisk htop inetutils less nodebrew openssh socat sommelier wget zsh'
DNSMASQ_VERSION='2.79'
LD_LIBRARY_PATH=/usr/local/lib:/usr/local/lib64:/lib:/lib64
LXC_BUILD_DIR=/tmp/lxc-src
LXC_DEPS='gcc7 gdb make libcap libseccomp libtool'
LXC_VERSION='3.0.2'
QEMU_DEPS='glib libepoxy libsdl2 mesa pixman'
QEMU_VERSION='3.0.0'
XSET_VERSION='1.2.4'

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

if [ '0' = "$(id -u)" ]; then
  echo This script should not be ran as root
  exit 1
fi

sudo mount -i -o remount,exec,suid $HOME
sudo mount -o remount,exec,suid /tmp

echo Add power manager overrides
# 4 hours
echo '14400000' | sudo tee /var/lib/power_manager/plugged_suspend_ms
sudo chown -R power:power /var/lib/power_manager/

echo Installing Chromebrew
if ! hash crew > /dev/null; then
  curl -Ls https://raw.github.com/skycocker/chromebrew/master/install.sh | bash
fi

echo Installing general tools from Chromebrew
echo "$CHROMEBREW_PACKAGES"
crew install $CHROMEBREW_PACKAGES

echo y | rw

echo Installing LXC Dependencies
echo "$LXC_DEPS"
crew install $LXC_DEPS

echo Installing LXC
if ! hash lxc-ls; then
  [[ -d "$LXC_BUILD_DIR" ]] || git clone https://github.com/lxc/lxc.git "$LXC_BUILD_DIR"
  cd "$LXC_BUILD_DIR"
  git checkout "lxc-$LXC_VERSION"

  ./autogen.sh
  ./configure
  make -j4
  sudo make LD_LIBRARY_PATH=$LD_LIBRARY_PATH install
fi

echo Installing dnsmasq
if ! hash dnsmasq; then
  [[ -f /tmp/dnsmasq.tar.xz ]] ||
    curl -SsL "http://www.thekelleys.org.uk/dnsmasq/dnsmasq-$DNSMASQ_VERSION.tar.xz" > /tmp/dnsmasq.tar.xz
  rm -rf /tmp/dnsmasq && cd /tmp && tar xf /tmp/dnsmasq.tar.xz
  cd "/tmp/dnsmasq-$DNSMASQ_VERSION"
  make -j4 PREFIX=/usr/local BINDIR=/usr/local/bin
  make install PREFIX=/usr/local BINDIR=/usr/local/bin
fi

install_qemu_tools() {
  local build_dir="qemu-${QEMU_VERSION}"
  local source_file="${build_dir}.tar.xz"

  crew install $QEMU_DEPS
  if ! hash qemu-img; then
    cd /tmp
    [[ -f /tmp/qemu.tar.xz ]] ||
      curl -SsLo "$source_file" "https://download.qemu.org/$source_file"
    rm -rf "$build_dir"
    mkdir "$build_dir"
    tar xf "$source_file"
    cd "$build_dir"

    ./configure --prefix=/usr/local \
      --disable-debug-tcg \
      --disable-gtk \
      --disable-xen \
      --enable-debug \
      --enable-debug-info \
      --enable-kvm \
      --enable-opengl \
      --enable-sdl \
      --disable-spice \
      --enable-vhost-net \
      --target-list=x86_64-softmmu
   make -j4
   make install
  fi
}

echo 'Installing QEMU tools (qemu-img qemu-nbd)'
install_qemu_tools
[[ -f /usr/local/etc/qemu-ifup ]] || cp "$DIR/qemu-ifup" /usr/local/etc/qemu-ifup
chmod +x /usr/local/etc/qemu-ifup

install_brctl() {
  if hash brctl; then
    return
  fi

  local build_dir="bridge-utils-${BRIDGE_UTILS_VERSION}"
  local source_file="${build_dir}.tar.xz"
  cd /tmp
  [[ -f "/tmp/$source_file" ]] || wget "https://www.kernel.org/pub/linux/utils/net/bridge-utils/$source_file"
  rm -rf "$build_dir"
  tar xf "$source_file"
  cd "$build_dir"

  autoconf
  ./configure
  make -j4
  make install
}

echo Installing brctl
install_brctl

download_initial_vm_disk() {
  if [[ -f /home/chronos/user/vms/archlinux.img ]]; then
    return
  fi

  # Get the previous fifth day of the month which is when
  # ArchLinux images are released on VagrantCloud
  local closest_fifth_day="2018.$(date -d '4 days ago' +%m).05"
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

echo Installing services
sudo cp upstart-services/arch-vm.conf /etc/init/arch-vm.conf
sudo cp upstart-services/crouton-clipboard.conf /etc/init/crouton-clipboard.conf
sudo cp upstart-services/forward-ports.conf /etc/init/forward-ports.conf
sudo cp upstart-services/sommelier-x1.conf /etc/init/sommelier-x1.conf
sudo cp upstart-services/sommelier-x2.conf /etc/init/sommelier-x2.conf

echo Installing Node.JS
nodebrew install 8 || true
nodebrew use 8 || true

echo Installing crouton-clipboard service
[[ -d /home/chronos/user/.crouton-clipboard/ ]] ||
  git clone https://github.com/zwhitchcox/crouton-clipboard.git /home/chronos/user/.crouton-clipboard/

echo Installing xset
if ! hash xset; then
  rm -rf /tmp/xset*
  cd /tmp
  curl -SsL "https://www.x.org/releases/individual/app/xset-${XSET_VERSION}.tar.gz" > /tmp/xset.tar.gz
  tar xf /tmp/xset.tar.gz
  cd "/tmp/xset-${XSET_VERSION}"
  autoconf
  ./configure
  make -j4
  make install
fi
