#!/bin/bash

set -xeuo pipefail

if [[ -z "${SPIN:-}" ]]; then
  echo Not a Spin installation, ignoring...

  exit 0
fi

sudo apt install -qy \
  autojump \
  bat \
  curl \
  homesick \
  htop \
  neovim \
  python3-pynvim \
  silversearcher-ag \
  tig \
  tmux \
  tree \
  zsh-autosuggestions \
  zsh-syntax-highlighting

curl -sL "https://github.com/junegunn/fzf/releases/download/v0.54.3/fzf-0.54.3-linux_amd64.tar.gz" | \
  sudo tar -xz -C /usr/local/bin

[[ -e /usr/bin/bat ]] || sudo ln -s /usr/bin/b{atc,}at

homesick clone cartolari/dotfiles
homesick link --force

vim +PlugInstall +qall
