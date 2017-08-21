#!/usr/bin/env bash

# Install development packages in an Arch Linux based OS

set -euo pipefail
IFS=$'\n\t'

arch_packages=(
ansible
arc-gtk-theme
aspell-en
aspell-pt
autojump
awless
aws-cli
bash
bash-completion
bfg
bind-tools
chrome-gnome-shell
chromium
chromium-widevine
cloc
cmake
copyq
curl
dbeaver
dnsmasq
docker-compose
doctl-bin
emacs
firefox
git
git-lfs-bin
global
gnome-boxes
gnome-clocks
gnome-terminal-transparency
gnome-weather
go
gpick
gvim
homesick
hosts-gen
htop
imagemagick
intellij-idea-community-edition
jdk8-openjdk
jq
libinput-gestures
moreutils
ncurses5-compat-libs
neovim
nodejs
noto-fonts
npm
oh-my-zsh-git
postgresql-libs
powerline
powerline-fonts-git
pv
pygmentize
python
python-pip
python2
python2-boto
python2-pip
ripgrep
ruby
s3cmd
sbt
scala
shellcheck
sqlite
sublime-text-dev
sysdig
terraform
the_silver_searcher
tig
tmux
tree
ttf-ubuntu-font-family
universal-ctags-git
unzip
vagrant
virtualbox-bin
virtualbox-ext-oracle
visual-studio-code
wget
wireshark-gtk
xclip
yarn
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
  pacaur -Qs "^$package$" > /dev/null
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
  pip install --user --upgrade "$package"
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

sudo usermod -aG docker "$USER"
sudo usermod -aG vboxusers "$USER"
sudo usermod -aG wireshark "$USER"
sudo usermod -aG input "$USER"
sudo chsh "$USER" --shell /bin/zsh
