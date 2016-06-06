#!/usr/bin/env bash

# set -euo pipefail
# # set -x
# IFS=$'\n\t'

packages=(
# android-sdk
# android-studio
# bash-concurrent
# bfg repo cleaner
# churn # rubygems
# eslint
# haml-lint
# https://github.com/zsh-users/zsh-completions
# jsctags
# jshint
# jsonlint
# pygments (ccat)
# python2.7-aws-shell
# rubocop
# ruby-install
# shellcheck
# ternjs
# webdev icon fonts

atom
autojump
bash
bash-completion
chromium
chruby
ctags # Preferably from ctags.io
curl
dnsmasq
docker-compose
emacs-25.0.93
firefox
git
global-6.5.4
go
homesick
htop
idea-community-2016.1.1
imagemagick
jq
letsencrypt-0.5.0
mercurial
moreutils
neovim
nix-zsh-completions
nodejs-6.1.0
openjdk # java
powerline-fonts
pv
python
python3
python3.5-awscli
ruby
silver-searcher
sqlite
sublimetext3
sysdig
tig
tmux # compiled from source
tree
unzip
vagrant
vim
virtualbox
wget
wireshark-gtk
xclip
zeal
zip
zsh
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

for package in "${packages[@]}"; do
  nix_install $package
done
