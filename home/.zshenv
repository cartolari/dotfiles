export XDG_BIN_HOME=$HOME/.local/bin
export XDG_CACHE_HOME=$HOME/.cache
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share
export XDG_LIB_HOME=$HOME/.local/lib

export GRML_ALWAYS_LOAD_ALL=1
export COMPDUMPFILE=$XDG_CACHE_HOME/zcompdump

export EDITOR=nvim

export DOCKER_CONFIG=$XDG_CONFIG_HOME/docker
export GEMRC=$XDG_CONFIG_HOME/gemrc
export LESSHISTFILE=$XDG_DATA_HOME/lesshst
export VAGRANT_HOME=$XDG_CONFIG_HOME/vagrant

export FZF_DEFAULT_COMMAND='ag --hidden -l -g ""'
export FZF_CTRL_T_COMMAND='ag --hidden -l -g ""'

export GOPATH=~/go
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"
export NVIM_TUI_ENABLE_TRUE_COLOR=1
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"
export PATH=$HOME/.local/bin:$PATH
export PATH=$PATH:$GOPATH/bin$file
export PATH=$PATH:$HOME/.gem/ruby/2.5.0/bin
export PATH=$PATH:$HOME/bin
export PATH=$PATH:$HOME/scripts
export PATH=$PATH:$XDG_DATA_HOME/npm/bin
export PATH=$PATH:/usr/bin/core_perl
export VIMINIT='let $MYVIMRC="$XDG_CONFIG_HOME/vim/vimrc" | source $MYVIMRC'
