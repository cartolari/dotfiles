#!/bin/bash

set -euo pipefail

AUGEAS_VERSION=1.11.0
BRIDGE_UTILS_VERSION=1.6
CHROMEBREW_PACKAGES='ag chromebrew_scripts gptfdisk htop inetutils less nodebrew openssh readline7 socat sommelier tmux wget zsh'
DNSMASQ_VERSION='2.79'
LD_LIBRARY_PATH=/usr/local/lib:/usr/local/lib64:/lib:/lib64
LXC_BUILD_DIR=/tmp/lxc-src
LXC_DEPS='gcc8 gdb make libcap libseccomp libtool'
LXC_VERSION='3.0.2'
QEMU_DEPS='glib libepoxy libsdl2 mesa pixman'
QEMU_VERSION='3.0.0'
TOUCHPAD_MINIMUM_PRESSURE='0.05'
XSET_VERSION='1.2.4'

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

if [ '0' = "$EUID" ]; then
  echo This script should not be ran as root
  exit 1
fi

sudo mount -i -o remount,exec,suid $HOME
sudo mount -o remount,exec,suid /tmp

if [[ "$(sudo passwd --status chronos | awk '{ print $2 }')" != "P" ]]; then
  echo Defining password for user chronos
  sudo passwd chronos
fi

if [[ "$(grep '^chronos:' /etc/passwd | cut -d: -f7)" != "/usr/local/bin/zsh" ]]; then
  echo Changing default shell to ZSH
  sudo chsh --shell /usr/local/bin/zsh chronos
fi

if [[ ! -f /etc/zsh/zshrc ]]; then
  sudo mkdir -p /etc/zsh
  sudo wget -O /etc/zsh/zshrc "https://git.grml.org/?p=grml-etc-core.git;a=blob_plain;f=etc/zsh/zshrc;hb=HEAD"
fi

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

echo Installing services
sudo "$(which rsync)" -azvpu "$DIR/upstart-services/" /etc/init

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

RCLONE_VERSION=1.45
echo Installing rclone
if ! hash rclone; then
  rm -rf /tmp/rclone*
  curl -SsL "https://github.com/ncw/rclone/releases/download/v$RCLONE_VERSION/rclone-v$RCLONE_VERSION-linux-amd64.zip" > /tmp/rclone.zip
  cd /tmp
  unzip rclone.zip
  cd "/tmp/rclone-v$RCLONE_VERSION-linux-amd64"
  cp rclone.1 /usr/local/share/man/man1/rclone.1
  cp rclone /usr/local/bin/rclone
fi

echo Installing augeas
if ! hash augtool; then
  rm -rf /tmp/augeas*
  cd /tmp
  curl -Ssl "http://download.augeas.net/augeas-${AUGEAS_VERSION}.tar.gz" > /tmp/augeas.tar.gz
  tar xf /tmp/augeas.tar.gz
  cd "/tmp/augeas-${AUGEAS_VERSION}"
  ./configure
  make -j4
  make install
fi

set +e
read -r -d '' AUGEAS_SCRIPT <<EOF
set etc/gesture/40-touchpad-cmt.conf/InputClass[Identifier = "CMT for Synaptics Touchpad"]/Option[. = "Tap Minimum Pressure"] "Tap Minimum Pressure"
set etc/gesture/40-touchpad-cmt.conf/InputClass[Identifier = "CMT for Synaptics Touchpad"]/Option[. = "Tap Minimum Pressure"]/value $TOUCHPAD_MINIMUM_PRESSURE
EOF
set -e

sudo env LD_LIBRARY_PATH=$LD_LIBRARY_PATH augtool -t 'Xorg incl /etc/gesture/*.conf' "$AUGEAS_SCRIPT"
