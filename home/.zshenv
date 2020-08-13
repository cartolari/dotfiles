export XDG_BIN_HOME=$HOME/.local/bin
export XDG_CACHE_HOME=$HOME/.cache
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share
export XDG_LIB_HOME=$HOME/.local/lib

if [[ -z "$XDG_RUNTIME_DIR" ]]; then
  export XDG_RUNTIME_DIR=/run/user/cartolari
fi

export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"
if hash apt &> /dev/null && [[ "$(ls -di /)" != "2" ]]; then
  # Inside a Chroot running Ubuntu, so we'll assume Chrome OS
  export SSH_AUTH_SOCK=/run/chrome/ssh-agent.socket
fi

export GRML_ALWAYS_LOAD_ALL=1
export COMPDUMPFILE=$XDG_CACHE_HOME/zcompdump

export EDITOR=nvim

export DOCKER_CONFIG=$XDG_CONFIG_HOME/docker
export GEMRC=$XDG_CONFIG_HOME/gemrc
export LESSHISTFILE=$XDG_DATA_HOME/lesshst
export VAGRANT_HOME=$XDG_CONFIG_HOME/vagrant

export FZF_DEFAULT_COMMAND='ag --hidden -l -g ""'
export FZF_CTRL_T_COMMAND='ag --hidden -l -g ""'

export LOCALE_ARCHIVE=/usr/lib64/locale/locale-archive

export GOPATH=~/go
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"
export NVIM_TUI_ENABLE_TRUE_COLOR=1

export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"
export PATH=$PATH:$GOPATH/bin$file
export PATH=$PATH:$HOME/.gem/ruby/2.7.0/bin
export PATH=$PATH:$HOME/bin
export PATH=$PATH:$HOME/scripts
export PATH=$PATH:$XDG_DATA_HOME/npm/bin
export PATH=$PATH:/usr/bin/core_perl
export PATH=$PATH:$HOME/.dotnet/tools

export MOZ_ENABLE_WAYLAND=1
export QT_QPA_PLATFORM=wayland
export _JAVA_AWT_WM_NONREPARENTING=1
export LS_COLORS=$LS_COLORS':ow=01;04;32'

export VIMINIT='let $MYVIMRC="$XDG_CONFIG_HOME/vim/vimrc" | source $MYVIMRC'
export DOCKER_HOST=tcp://localhost:2376
