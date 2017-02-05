#!/usr/bin/env bash

# Install development packages in an Arch Linux based OS

set -euo pipefail
IFS=$'\n\t'

arch_packages=(
android-sdk
android-studio
ansible
aspell-en
aspell-pt
atom-editor-bin
autojump
aws-cli
bash
bash-completion
bfg
bind-tools
chromium
chruby
cmake
curl
dbeaver
dnsmasq
docker-compose
doctl-bin
emacs
firefox
git
git-lfs
global
go
gvim
homesick
hosts-gen
htop
imagemagick
intellij-idea-community-edition
jdk8-openjdk
jq
letsencrypt-cli
mercurial
moreutils
mycli
ncurses5-compat-libs
neovim
nodejs
npm
oh-my-zsh
postgresql-libs
powerline-fonts-git
pv
pygmentize
python
python-pip
python2
python2-pip
ripgrep
ruby
ruby-install
shellcheck
sqlite
sublime-text-dev
sysdig
terraform-bin
the_silver_searcher
tig
tmux-git
tree
universal-ctags-git
unzip
vagrant
virtualbox-bin
virtualbox-ext-oracle
wget
wireshark-qt
xclip
zeal
zip
zsh
zsh-autosuggestions
zsh-completions
zsh-syntax-highlighting
)

python_packages=(
aws-shell
neovim
pgcli
ptpython
py-mini-racer
vim-vint
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

for service in systemd-units/**/*.service; do
  unit_name=$(basename "$service")
  source=$(realpath "$service")
  target=/etc/systemd/system/$unit_name
  if [ -h "$target" ] && [ "$(readlink -f "$target")" = "$source" ]; then
    continue
  fi
  sudo ln -s "$source" "$target"
done
