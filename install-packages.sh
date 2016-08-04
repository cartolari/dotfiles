#!/usr/bin/env bash

# Install development packages in an Arch Linux based OS

set -euo pipefail
IFS=$'\n\t'

arch_packages=(
android-sdk
android-studio
aspell-en
aspell-pt
atom-editor-bin
autojump
aws-cli
bash
bash-completion
bfg
chromium
chruby
curl
dnsmasq
docker-compose
emacs
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
nodejs
npm
postgresql-libs
powerline-fonts-git
pv
pygmentize
pygmentize
python
python2
ruby
ruby-install
shellcheck
sqlite
sublime-text-dev
sysdig
the_silver_searcher
tig
tmux-git
tree
universal-ctags-git
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
zsh-autosuggestions
zsh-completions
)

python_packages=(
aws-shell
neovim
pgcli
)

pacaur_install () {
  package=$1
  echo "Installing $package"
  set +e
  pacaur -Qs "$package" > /dev/null
  if [[ $? -eq 0 ]]; then
    return
  fi
  set -e
  pacaur -S --noconfirm --noedit "$package"
}

for package in "${arch_packages[@]}"; do
  pacaur_install "$package"
done

for package in "${python_packages[@]}"; do
  pip install --user "$package"
done
