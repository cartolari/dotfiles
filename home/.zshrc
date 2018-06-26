unsetopt nomatch

export XDG_BIN_HOME=$HOME/.local/bin
export XDG_CACHE_HOME=$HOME/.cache
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share
export XDG_LIB_HOME=$HOME/.local/lib

export COMPLETION_WAITING_DOTS="true"
export DISABLE_AUTO_TITLE="true"
export DISABLE_AUTO_UPDATE="true"
export ENABLE_CORRECTION="true"
export HYPHEN_INSENSITIVE="true"
export UPDATE_ZSH_DAYS=7
export ZSH_CACHE_DIR=$XDG_CACHE_HOME/zsh
export ZSH_THEME="agnoster"

plugins=(
  autojump
  aws
  command-not-found
  docker
  docker-compose
  git
  systemd
  terraform
  vagrant
)

man() {
  LESS_TERMCAP_md=$'\e[01;31m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[01;44;33m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[01;32m' \
    command man "$@"
}

export DOCKER_CONFIG=$XDG_CONFIG_HOME/docker
export GEMRC=$XDG_CONFIG_HOME/gemrc
export LESSHISTFILE=$XDG_DATA_HOME/lesshst
export VAGRANT_HOME=$XDG_CONFIG_HOME/vagrant
fc -p $XDG_DATA_HOME/zsh_history

export GOPATH=~/go
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"
export PATH=$PATH:$XDG_DATA_HOME/npm/bin
export PATH=$PATH:$HOME/bin
export PATH=$PATH:$HOME/scripts
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$HOME/.gem/ruby/2.5.0/bin
export PATH=$PATH:/usr/bin/core_perl
export PATH=$HOME/.local/bin:$PATH
export VIMINIT='let $MYVIMRC="$XDG_CONFIG_HOME/vim/vimrc" | source $MYVIMRC'

ZSH_CUSTOM=$XDG_CONFIG_HOME/zsh-custom

export ZSH=/usr/share/oh-my-zsh
source $ZSH/oh-my-zsh.sh

# Disable software control flow
stty -ixon

export EDITOR=nvim

if [[ -z "$TMUX" ]]; then
  tmux attach-session -t default || tmux new-session -s default
fi
if [[ -n ${INSIDE_EMACS} ]]; then
  # This shell runs inside an Emacs *shell*/*term* buffer.
  unsetopt zle
fi

export FZF_DEFAULT_COMMAND='ag -l -g ""'
[ -f /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh
[ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh

export NVIM_TUI_ENABLE_TRUE_COLOR=1

source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
