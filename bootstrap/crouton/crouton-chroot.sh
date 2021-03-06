#!/bin/bash

set -xeuo pipefail

FZF_VERSION=0.18.0
RIPGREP_VERSION=11.0.1
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
  cmake \
  exuberant-ctags \
  fzf \
  git \
  git-lfs \
  golang-go \
  homesick \
  htop \
  inotify-tools \
  jq \
  libncurses5-dev \
  lxc-utils \
  lxd \
  net-tools \
  ocaml-nox \
  openconnect \
  postgresql-client-10 \
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
  sshpass \
  tig \
  tmux \
  tree \
  vagrant \
  vim-gtk \
  zsh \
  zsh-autosuggestions \
  zsh-syntax-highlighting

sudo apt install -y -o Dpkg::Options::="--force-overwrite" bat ripgrep
[[ -e /usr/bin/bat ]] || sudo ln -s /usr/bin/batcat /usr/bin/bat

cat <<EOF | sudo tee /etc/ssh/sshd_config.d/crouton.conf
HostKey /etc/ssh/ssh_host_rsa_key
HostKey /etc/ssh/ssh_host_ecdsa_key
HostKey /etc/ssh/ssh_host_ed25519_key

ListenAddress 127.0.0.1
Port 2223
EOF

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

echo NodeJS Setup
curl -sL https://deb.nodesource.com/setup_current.x | sudo -E bash -
sudo apt install -y nodejs

if ! hash docker-compose; then
  curl \
    -L \
    "https://github.com/docker/compose/releases/download/1.24.0/docker-compose-$(uname -s)-$(uname -m)" | \
    sudo install -T /dev/stdin /usr/local/bin/docker-compose
fi

sudo cp "$DIR/rc.local" /etc/rc.local

# Clustergit
[[ -f ~/.local/bin/clustergit ]]  ||
  curl -SsL https://raw.githubusercontent.com/mnagel/clustergit/master/clustergit | \
  install -m 0755 /dev/stdin ~/.local/bin/clustergit
