#!/usr/bin/env bash

# set -euo pipefail
# # set -x
# IFS=$'\n\t'

# pip install pygments
# pygments (ccat)

packages=(
# bash-concurrent
# churn # rubygems
# eslint
# haml-lint
# jsctags
# jsonlint
# python2.7-aws-shell
# rubocop
# ternjs
# webdev icon fonts
android-sdk
# android-studio
atom-editor-beta-bin
autojump
aws-cli
bash
bash-completion
bfg
chromium
chruby
ctags # Preferably from ctags.io
curl
dnsmasq
docker-compose
# emacs-git
firefox
git
global
go
homesick
htop
imagemagick
intellij-idea-community-edition
jdk8-openjdk
jq
letsencrypt-cli
mercurial
moreutils
neovim
nix-zsh-completions
nodejs
npm
powerline-fonts
pv
python
python2
ruby
ruby-install
shellcheck
silver-searcher
sqlite
sublime-text-dev
sysdig
tig
tmux-git
tree
unzip
vagrant
vim
virtualbox
wget
wireshark-qt
xclip
zeal
zip
zsh
zsh-completions
)

nix_install () {
  package=$1
  set +e
  nix-env -q "$package" > /dev/null
  if [ $? -eq 0 ]; then
    echo "Skipping $package"
    return
  fi
  set -e
  echo "Installing $package"

  nix-env --install --prebuilt-only "$package"
}

pacaur_install () {
  package=$1
  echo "Installing $package"
  set +e
  pacaur -S --needed --noconfirm --noedit "$package"
  set -e
}

for package in "${packages[@]}"; do
  pacaur_install $package
done
