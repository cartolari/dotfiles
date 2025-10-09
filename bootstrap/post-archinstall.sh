#!/bin/bash

set -xeuo pipefail

if [ "$EUID" -eq 0 ]; then
  echo This script should not be executed as root
  exit 1
fi

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
  autojump \
  aws-cli-v2 \
  bat \
  cloc \
  docker-compose \
  find-the-command \
  fish \
  flatpak \
  fslint \
  fzf \
  go \
  gocryptfs \
  graphviz \
  grml-zsh-config \
  htop \
  imagemagick \
  jdk-openjdk \
  jq \
  linux-headers \
  moreutils \
  neovim \
  net-tools \
  nodejs \
  npm \
  oath-toolkit \
  otf-font-awesome \
  python \
  python-boto \
  python-pynvim \
  qemu-desktop \
  rclone \
  ripgrep \
  rmlint \
  ruby \
  s3cmd \
  shellcheck-bin \
  sqlite \
  sysstat \
  terraform \
  the_silver_searcher \
  tig \
  tmux \
  tree \
  ufw \
  units \
  unzip \
  vagrant \
  vim \
  virt-manager \
  virt-viewer \
  visual-studio-code-bin \
  yarn \
  zip \
  zsh \
  zsh-autosuggestions \
  zsh-completions \
  zsh-syntax-highlighting

sudo chsh --shell /bin/fish "$USER"
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
