#!/bin/bash

set -xeuo pipefail

FZF_VERSION=0.18.0
RIPGREP_VERSION=11.0.1
UNISON_VERSION=2.48.4
WHICH_VERSION=2.21

if [ "$EUID" -eq 0 ]; then
  echo This script should not be executed as root
  exit 1
fi

echo Installing essential packages
sudo apt update
sudo apt install -y \
  build-essential \
  curl \
  wget

echo Building Which
mkdir -p ~/.local/bin
if [[ ! -f ~/.local/bin/which ]]; then
	curl -SsL \
    http://ftp.gnu.org/gnu/which/which-${WHICH_VERSION}.tar.gz | \
    tar zxv -C /tmp
  cd /tmp/which-${WHICH_VERSION}
  ./configure
  make
  cp ./which ~/.local/bin/which
fi

echo PostgreSQL repos
[[ -f /etc/apt/sources.lists.d/pgdg.list ]] ||
  echo 'deb http://apt.postgresql.org/pub/repos/apt/ bionic-pgdg main' | \
  sudo tee /etc/apt/sources.list.d/pgdg.list

curl -SsL https://www.postgresql.org/media/keys/ACCC4CF8.asc | \
  sudo apt-key add -

echo Installing packages
sudo apt install -y \
  autojump \
  cloc \
  exuberant-ctags \
  git \
  git-lfs \
  homesick \
  htop \
  inotify-tools \
  jq \
  libncurses5-dev \
  lxc-utils \
  lxd \
  ocaml-nox \
  openconnect \
  postgresql-client-10 \
  python \
  python-pip \
  python3 \
  python3-pip \
  qemu-kvm \
  rclone \
  rmlint \
  ruby \
  s3cmd \
  shellcheck \
  silversearcher-ag \
  socat \
  sqlite \
  tig \
  tmux \
  tree \
  vagrant \
  zsh \
  zsh-syntax-highlighting

echo ZSH Setup
sudo mkdir -p /etc/zsh
sudo touch /etc/zsh/zshrc
if ! grep -q grml /etc/zsh/zshrc; then
  sudo curl -SsLo /etc/zsh/zshrc https://git.grml.org/f/grml-etc-core/etc/zsh/zshrc
fi

echo Docker Setup
if ! hash docker; then
  curl -SsL https://get.docker.com | sh
fi

if ! hash docker-compose; then
  curl \
    -L \
    "https://github.com/docker/compose/releases/download/1.24.0/docker-compose-$(uname -s)-$(uname -m)" | \
    sudo install -T /dev/stdin /usr/local/bin/docker-compose
fi

if ! hash docker-machine; then
  curl -L "https://github.com/docker/machine/releases/download/v0.16.1/docker-machine-$(uname -s)-$(uname -m)" |
    sudo install -T /dev/stdin /usr/local/bin/docker-machine
fi

hash -r

echo Unison Install
if [[ ! -f ~/.local/bin/unison ]]; then
	curl -L \
    https://github.com/bcpierce00/unison/archive/${UNISON_VERSION}.tar.gz | \
    tar zxv -C /tmp
	cd /tmp/unison-${UNISON_VERSION}
	sed -i -e \
    's/GLIBC_SUPPORT_INOTIFY 0/GLIBC_SUPPORT_INOTIFY 1/' \
    src/fsmonitor/linux/inotify_stubs.c
	make UISTYLE=text NATIVE=true STATIC=true

  mkdir -p ~/.local/bin
	cp src/unison src/unison-fsmonitor ~/.local/bin
fi

echo Docker VM Setup
if ! docker-machine ls --filter name=default | grep -q '^default'; then
  docker-machine create \
    --driver virtualbox \
    --virtualbox-cpu-count 2 \
    --virtualbox-disk-size 40000 \
    --virtualbox-memory 4096 \
    default
fi

if ! docker-machine ls --filter name=default --filter state=Running | grep -q '^default'; then
  docker-machine start default
fi

docker-machine scp -q ~/.local/bin/unison default:~/
docker-machine scp -q ~/.local/bin/unison-fsmonitor default:~/
docker-machine ssh default mkdir -p "/home/$USER/default"

# shellcheck disable=SC2088
docker-machine ssh default sudo cp '~/unison*' /usr/local/bin

echo Unison Config Setup

# Unison config
mkdir -p ~/.unison

cat <<EOF > ~/.unison/docker.prf
auto = true
batch = true
confirmbigdel=true
repeat = watch

root = /home/$USER/code
root = socket://192.168.99.101:9090//home/$USER/code
EOF

cat <<'EOF' > /tmp/unison-socket-daemon
#!/bin/sh

start() {
  if [ -e /var/run/unison.pid ]; then
    echo Unison is already running
    exit 0
  fi
  start-stop-daemon --start --exec /usr/local/bin/unison -socket 9090 &> /tmp/unison.log
}

stop() {
  kill $(pidof unison)
}

status() {
  if [ -e /var/run/unison.pid ]; then
    echo -e "\nunison is running.\n"
    exit 0
  else
    echo -e "\nunison is not running.\n"
    exit 1
  fi
}

case $1 in
  start) start
    ;;
  stop) stop
    ;;
  status) status
    ;;
  restart) stop; start
    ;;
  *) echo -e "\n$0 [start|stop|restart|status]\n"
    ;;
esac
EOF

chmod +x /tmp/unison-socket-daemon
docker-machine scp /tmp/unison-socket-daemon default:/tmp/unison
docker-machine ssh default sudo mv /tmp/unison /usr/local/etc/init.d/unison

echo FZF
if ! hash fzf; then
  sudo curl -SsLo /usr/local/bin/fzf \
    https://github.com/junegunn/fzf-bin/releases/download/$FZF_VERSION/fzf-$FZF_VERSION-linux_amd64.tgz
fi

if ! hash rg; then
  curl -LO https://github.com/BurntSushi/ripgrep/releases/download/$RIPGREP_VERSION/ripgrep_${RIPGREP_VERSION}_amd64.deb
  sudo dpkg -i ripgrep_${RIPGREP_VERSION}_amd64.deb
  rm ripgrep_${RIPGREP_VERSION}_amd64.deb
fi

# zsh-autosuggestions
