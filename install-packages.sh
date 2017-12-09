#!/usr/bin/env bash

# Install development packages in an Arch Linux based OS

desired_mirrors=United_States
enabled_mirrors=$(
grep Country < /etc/pacman.d/mirrorlist | \
  grep -oP '\w+$' | \
  sort -u | \
  paste -sd,
)

if [ "$desired_mirrors" != "$enabled_mirrors" ]; then
  sudo rankmirrors -f 0 -c "$desired_mirrors"
fi

ensure_gpg_key() {
  local server=$1
  local key_id=$2

  gpg --list-keys "$key_id" > /dev/null || \
    gpg --keyserver "$server" --recv-keys "$key_id"
}

# Dave Reisner, required for Cower a Pacaur dependency
ensure_gpg_key hkp://pgp.mit.edu 1EB2638FF56C0C53
# Thomas Dickey, required for ncurses5-compat-libs
ensure_gpg_key hkp://pgp.mit.edu 702353E0F7E48EDB

set -euo pipefail
IFS=$'\n\t'

arch_packages=(
ansible
arc-gtk-theme
aspell-en
aspell-pt
autojump
aws-cli
bash
bash-completion
bind-tools
chrome-gnome-shell
chromium-vaapi-bin
chromium-widevine
cloc
cmake
curl
dbeaver
deja-dup
dnsmasq
docker-compose-bin
doctl-bin
ebtables
emacs
firefox
font-awesome
git
git-lfs-bin
global
gnome-boxes
gnome-clocks
gnome-mpv
gnome-terminal-transparency
gnome-weather
go
gpick
gvim
homesick
hosts-gen
htop
imagemagick
jdk8-openjdk
jq
libinput-gestures
megasync
nautilus-megasync
meld
moreutils
ncurses5-compat-libs
neovim
nodejs
noto-fonts
npm
nylas-mail-lives-bin
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
remmina
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
virt-manager
virtio-win
virtualbox
virtualbox-ext-oracle
virtualbox-host-dkms
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

npm_packages=(
base16-builder
)

pacaur_install () {
  package=$1
  echo "Installing $package"
  set +e

  if pacaur -Qs "^$package$" > /dev/null; then
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

for package in "${npm_packages[@]}"; do
  if ! npm --global list | grep -q "$package"; then
    npm install --global "$package"
  fi
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
libinput-gestures-setup autostart
