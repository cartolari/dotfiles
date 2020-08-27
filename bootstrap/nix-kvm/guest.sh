#!/bin/bash

set -euo pipefail

if [ "$EUID" -eq 0 ]; then
  echo This script should not be executed as root
  exit 1
fi

echo "Update Pacman mirrors?"
select yn in "yes" "no"; do
  case $yn in
    yes )
      curl 'https://www.archlinux.org/mirrorlist/?country=BR&protocol=http&ip_version=4&use_mirror_status=on' | \
        sed 's/^#Server =/Server =/' | \
        sudo tee /etc/pacman.d/mirrorlist
      break ;;
    no ) echo "Skipping mirror update"; break ;;
  esac
done

echo "Update and upgrade?"
select yn in "yes" "no"; do
  case $yn in
    yes ) sudo pacman -Syyu; break ;;
    no ) echo "Skipping update and upgrade"; break ;;
  esac
done

sudo pacman -S --needed --noconfirm base base-devel git

if ! hash yay; then
  pushd /tmp
  git clone https://aur.archlinux.org/yay-bin.git
  cd yay-bin
  makepkg --noconfirm -si
  sudo pacman -U --noconfirm yay-bin-*.tar.*
  popd
fi

yay -S --needed --noconfirm \
  aspell-en \
  aspell-pt \
  autojump \
  aws-cli \
  bat \
  cloc \
  clustergit-git \
  cmake \
  code \
  docker-compose \
  ebtables \
  emacs \
  expect \
  find-the-command \
  fish \
  flatpak \
  fslint \
  fzf \
  git-lfs \
  global \
  go \
  grml-zsh-config \
  gufw \
  htop \
  imagemagick \
  jdk-openjdk \
  linux-headers \
  meld \
  moreutils \
  neovim \
  net-tools \
  nodejs \
  npm \
  oath-toolkit \
  openbox \
  openconnect \
  packer \
  pgadmin4 \
  postgresql-libs \
  powerline \
  python \
  python-boto \
  python-neovim \
  python-pip \
  python-psutil \
  python-pygments \
  qemu \
  qemu-guest-agent \
  rclone \
  ripgrep \
  rmlint \
  ruby \
  s3cmd \
  sbt \
  scala \
  shellcheck-bin \
  sqlite \
  sysdig \
  sysstat \
  terraform \
  the_silver_searcher \
  tig \
  tigervnc \
  tmux \
  tree \
  ufw \
  units \
  unzip \
  vagrant \
  vim \
  virt-manager \
  virt-viewer \
  xclip \
  yarn \
  zsh \
  zsh-autosuggestions \
  zsh-completions \
  zsh-syntax-highlighting

sudo chsh --shell /bin/zsh "$USER"
sudo usermod -aG docker "$USER"
sudo mkdir -p /etc/docker
if [[ ! -e /etc/docker/daemon.json ]]; then
  cat <<EOF | sudo tee /etc/docker/daemon.json > /dev/null
  {
    "default-address-pools" : [
      {
        "base" : "172.240.0.0/16",
        "size" : 24
      }
    ]
  }
EOF
fi
sudo systemctl enable --now docker
