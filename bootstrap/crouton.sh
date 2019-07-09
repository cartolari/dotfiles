#!/bin/bash

set -xeuo pipefail

FZF_VERSION=0.18.0
RIPGREP_VERSION=11.0.1
UNISON_VERSION=2.48.4
WHICH_VERSION=2.21

if [ "$EUID" -eq 0 ]; then
  echo This script should not be executed as root
  exit 1
fi

DIR="$( cd "$(dirname "$0")" ; pwd -P  )"

echo Installing essential packages
sudo apt update
sudo apt install -y \
  build-essential \
  curl \
  wget

echo Building Which
mkdir -p ~/.local/bin
if [[ ! -f ~/.local/bin/which ]]; then
	curl -SsL \
    http://ftp.gnu.org/gnu/which/which-${WHICH_VERSION}.tar.gz | \
    tar zxv -C /tmp
  cd /tmp/which-${WHICH_VERSION}
  ./configure
  make
  cp ./which ~/.local/bin/which
fi

echo PostgreSQL repos
[[ -f /etc/apt/sources.lists.d/pgdg.list ]] ||
  echo 'deb http://apt.postgresql.org/pub/repos/apt/ bionic-pgdg main' | \
  sudo tee /etc/apt/sources.list.d/pgdg.list

curl -SsL https://www.postgresql.org/media/keys/ACCC4CF8.asc | \
  sudo apt-key add -

echo Installing packages
sudo apt install -y \
  autojump \
  cloc \
  exuberant-ctags \
  git \
  git-lfs \
  homesick \
  htop \
  inotify-tools \
  jq \
  libncurses5-dev \
  lxc-utils \
  lxd \
  ocaml-nox \
  openconnect \
  postgresql-client-10 \
  python \
  python-pip \
  python3 \
  python3-pip \
  qemu-kvm \
  rclone \
  rmlint \
  ruby \
  s3cmd \
  shellcheck \
  silversearcher-ag \
  socat \
  sqlite \
  tig \
  tmux \
  tree \
  vagrant \
  zsh \
  zsh-syntax-highlighting

echo ZSH Setup
sudo mkdir -p /etc/zsh
sudo touch /etc/zsh/zshrc
if ! grep -q grml /etc/zsh/zshrc; then
  sudo curl -SsLo /etc/zsh/zshrc https://git.grml.org/f/grml-etc-core/etc/zsh/zshrc
fi

echo Docker Setup
if ! hash docker; then
  curl -SsL https://get.docker.com | sh
fi

if ! hash docker-compose; then
  curl \
    -L \
    "https://github.com/docker/compose/releases/download/1.24.0/docker-compose-$(uname -s)-$(uname -m)" | \
    sudo install -T /dev/stdin /usr/local/bin/docker-compose
fi

mkdir -p ~/.local/share/docker-vm
cd ~/.local/share/docker-vm
if [[ ! -f bionic-server-cloudimg-amd64.img ]]; then
  curl -L https://cloud-images.ubuntu.com/bionic/current/bionic-server-cloudimg-amd64.img > bionic-server-cloudimg-amd64.img
fi

if [[ ! -f disk.img ]]; then
  qemu-img create -b bionic-server-cloudimg-amd64.img -f qcow2 disk.img
fi

if [[ ! -f seed.iso ]]; then
  (
    cd "$DIR/docker-vm/seed"
    genisoimage -output seed.iso -volid cidata -joliet -rock .
    mv seed.iso ~/.local/share/docker-vm/seed.iso
  )
fi

hash -r

echo Unison Install
if [[ ! -f ~/.local/bin/unison ]]; then
	curl -L \
    https://github.com/bcpierce00/unison/archive/${UNISON_VERSION}.tar.gz | \
    tar zxv -C /tmp
	cd /tmp/unison-${UNISON_VERSION}
	sed -i -e \
    's/GLIBC_SUPPORT_INOTIFY 0/GLIBC_SUPPORT_INOTIFY 1/' \
    src/fsmonitor/linux/inotify_stubs.c
	make UISTYLE=text NATIVE=true STATIC=true

  mkdir -p ~/.local/bin
	cp src/unison src/unison-fsmonitor ~/.local/bin
fi

echo Unison Config Setup
# Unison config
mkdir -p ~/.unison

cat <<EOF > ~/.unison/docker.prf
auto = true
batch = true
confirmbigdel=true
repeat = watch

root = /home/$USER/code
root = socket://10.0.2.2:9090//home/$USER/code
EOF

echo FZF
if ! hash fzf; then
  sudo curl -SsLo /usr/local/bin/fzf \
    https://github.com/junegunn/fzf-bin/releases/download/$FZF_VERSION/fzf-$FZF_VERSION-linux_amd64.tgz
fi

if ! hash rg; then
  curl -LO https://github.com/BurntSushi/ripgrep/releases/download/$RIPGREP_VERSION/ripgrep_${RIPGREP_VERSION}_amd64.deb
  sudo dpkg -i ripgrep_${RIPGREP_VERSION}_amd64.deb
  rm ripgrep_${RIPGREP_VERSION}_amd64.deb
fi

# zsh-autosuggestions
