#!/bin/bash

set -xeuo pipefail

if [[ -z "${SPIN:-}" ]]; then
  echo Not a Spin installation, ignoring...

  exit 0
fi

sudo apt install -qy \
  autojump \
  bat \
  fzf \
  homesick \
  htop \
  silversearcher-ag \
  tig \
  tmux \
  tree \
  zsh-autosuggestions \
  zsh-syntax-highlighting

[[ -e /usr/bin/bat ]] || sudo ln -s /usr/bin/b{atc,}at

homesick clone cartolari/dotfiles
homesick link --force
